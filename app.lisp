;;;; app.lisp — Expert system (Common Lisp + SQLite CLI + Web UI)

;; Load Quicklisp
(load "/root/quicklisp/setup.lisp")
(ql:quickload '(:hunchentoot :cl-json :uiop))

(defpackage :expert
  (:use :cl :hunchentoot))
(in-package :expert)

;; -------------------------
;; CONFIG
;; -------------------------

(defparameter *db-path* "/app/data/expert.db")

(defun env (name &optional (default "10000"))
  (or (uiop:getenv name) default))

(defun port ()
  (parse-integer (env "PORT" "10000")))

;; -------------------------
;; SQLITE HELPERS (CLI)
;; -------------------------

(defun sql (query)
  "Execute an SQLite query and return lines."
  (uiop:run-program
   (list "sqlite3" *db-path* query)
   :output :lines))

(defun sql-exec (query)
  (uiop:run-program
   (list "sqlite3" *db-path* query)
   :output t))

;; -------------------------
;; DB INIT
;; -------------------------

(defun init-db ()
  (sql-exec "
    CREATE TABLE IF NOT EXISTS facts (
      code TEXT PRIMARY KEY,
      label TEXT
    );
  ")
  (sql-exec "
    CREATE TABLE IF NOT EXISTS rules (
      id TEXT PRIMARY KEY,
      conds TEXT,
      conclusion TEXT
    );
  ")

  ;; Seed minimal data
  (sql-exec "INSERT OR IGNORE INTO facts VALUES ('F1','PC lent');")
  (sql-exec "INSERT OR IGNORE INTO facts VALUES ('F11','Température CPU élevée');")
  (sql-exec "INSERT OR IGNORE INTO facts VALUES ('F12','Poussière dans le système');")

  (sql-exec "
    INSERT OR IGNORE INTO rules VALUES
    ('R9','F1,F11,F12','CAUSE_POSSIBLE(pc_lent,surchauffe)');
  ")
  (sql-exec "
    INSERT OR IGNORE INTO rules VALUES
    ('R10','CAUSE_POSSIBLE(pc_lent,surchauffe)','ACTION_RECOMMANDEE(pc_lent,Nettoyer le refroidissement)');
  "))

;; -------------------------
;; LOAD FACTS / RULES
;; -------------------------

(defun load-facts ()
  (mapcar
   (lambda (l)
     (destructuring-bind (code label)
         (uiop:split-string l :separator "|")
       (list :code code :label label)))
   (sql "SELECT code||'|'||label FROM facts;")))

(defun load-rules ()
  (mapcar
   (lambda (l)
     (destructuring-bind (id conds then)
         (uiop:split-string l :separator "|")
       (list :id id
             :conds (uiop:split-string conds :separator ",")
             :then then)))
   (sql "SELECT id||'|'||conds||'|'||conclusion FROM rules;")))

;; -------------------------
;; FORWARD CHAINING
;; -------------------------

(defun forward-chain (facts)
  (let ((known (copy-list facts))
        (trace '())
        (rules (load-rules))
        (changed t))
    (loop while changed do
      (setf changed nil)
      (dolist (r rules)
        (when (and
               (every (lambda (c) (member c known :test #'string=))
                      (getf r :conds))
               (not (member (getf r :then) known :test #'string=)))
          (push (getf r :then) known)
          (push (format nil "~A fired -> ~A" (getf r :id) (getf r :then)) trace)
          (setf changed t))))
    (list
     :facts known
     :trace (nreverse trace)
     :actions (remove-if-not
               (lambda (f) (search "ACTION_RECOMMANDEE" f))
               known))))

;; -------------------------
;; BACKWARD CHAINING
;; -------------------------

(defun backward-prove (goal facts)
  (cond
    ((member goal facts :test #'string=)
     (list :proven t :trace (list "Goal already known")))
    (t
     (let ((rules (remove-if-not
                   (lambda (r) (string= (getf r :then) goal))
                   (load-rules))))
       (dolist (r rules (list :proven nil :trace (list "No rule proves goal")))
         (when (every (lambda (c) (member c facts :test #'string=))
                      (getf r :conds))
           (return
             (list :proven t
                   :trace (list
                           (format nil "Used ~A" (getf r :id))
                           "All conditions satisfied")))))))))

;; -------------------------
;; HTTP API
;; -------------------------

(defun json (obj)
  (setf (content-type*) "application/json")
  (cl-json:encode-json-to-string obj))

(define-easy-handler (health :uri "/healthz") ()
  "ok")

(define-easy-handler (api-facts :uri "/api/facts") ()
  (json (list :facts (load-facts))))

(define-easy-handler (api-run :uri "/api/diagnostic") ()
  (let* ((p (cl-json:decode-json-from-string
             (raw-post-data :force-text t)))
         (facts (cdr (assoc :facts p)))
         (mode (cdr (assoc :mode p)))
         (goal (cdr (assoc :goal p))))
    (json
     (if (string= mode "backward")
         (backward-prove goal facts)
         (forward-chain facts)))))

;; -------------------------
;; STATIC FILES
;; -------------------------

(push (create-folder-dispatcher-and-handler "/static/" #p"/app/static/")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/" #p"/app/static/index.html")
      *dispatch-table*)

;; -------------------------
;; START SERVER
;; -------------------------

(init-db)
(start (make-instance 'easy-acceptor :port (port)))
(loop (sleep 3600))