;;;; app.lisp — Common Lisp Expert System (SQLite + HTTP API + static frontend)
;;;; Dependencies: hunchentoot, cl-json, dbi, dbd-sqlite3, uiop
;; Charger Quicklisp si présent (Docker/Render)
(let ((ql-setup #p"/root/quicklisp/setup.lisp"))
  (when (probe-file ql-setup)
    (load ql-setup)))

    
(ql:quickload '(:hunchentoot :cl-json :dbi :dbd-sqlite3 :uiop))

(defpackage :expert
  (:use :cl :hunchentoot))
(in-package :expert)

;;; ----------------------------
;;; 1) Helpers: env, JSON, etc.
;;; ----------------------------

(defun env (name &optional (default nil))
  (or (uiop:getenv name) default))

(defun listen-port ()
  (parse-integer (or (env "PORT" "10000") "10000") :junk-allowed t))

(defun json-response (plist &key (code 200))
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:return-code*) code)
  (cl-json:encode-json-to-string plist))

(defun read-json-body ()
  (let ((raw (hunchentoot:raw-post-data :force-text t)))
    (if (and raw (> (length raw) 0))
        (cl-json:decode-json-from-string raw)
        nil)))

(defun now-iso ()
  (multiple-value-bind (sec min hour day mon year) (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year mon day hour min sec)))

;;; ----------------------------
;;; 2) SQLite access (DBI)
;;; ----------------------------

(defparameter *db-path* (or (env "DB_PATH" nil) "/app/data/expert.db"))
(defvar *conn* nil)

(defun db-connect ()
  (unless *conn*
    (setf *conn* (dbi:connect :sqlite3 :database-name *db-path*)))
  *conn*)

(defun db-exec (sql &rest params)
  (let* ((conn (db-connect))
         (stmt (dbi:prepare conn sql)))
    (unwind-protect
         (progn
           (apply #'dbi:execute stmt params)
           t)
      (ignore-errors (dbi:close stmt)))))

(defun db-all (sql &rest params)
  (let* ((conn (db-connect))
         (stmt (dbi:prepare conn sql)))
    (unwind-protect
         (progn
           (apply #'dbi:execute stmt params)
           (loop for row = (dbi:fetch stmt)
                 while row
                 collect row))
      (ignore-errors (dbi:close stmt)))))

(defun db-one (sql &rest params)
  (first (apply #'db-all sql params)))

;;; ----------------------------
;;; 3) Schema + seed data
;;; ----------------------------

(defun ensure-schema ()
  ;; Facts: code + label
  (db-exec
   "CREATE TABLE IF NOT EXISTS facts (
      code TEXT PRIMARY KEY,
      label TEXT NOT NULL
    );")
  ;; Rules: id, conditions JSON array, conclusion string
  (db-exec
   "CREATE TABLE IF NOT EXISTS rules (
      id TEXT PRIMARY KEY,
      conds_json TEXT NOT NULL,
      conclusion TEXT NOT NULL
    );")
  ;; Optional: small log table (useful in demo)
  (db-exec
   "CREATE TABLE IF NOT EXISTS runs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      mode TEXT NOT NULL,
      created_at TEXT NOT NULL,
      facts_json TEXT NOT NULL,
      goal TEXT
    );"))

(defun seed-if-empty ()
  (let ((facts-count (getf (db-one "SELECT COUNT(*) AS c FROM facts;") :|c|))
        (rules-count (getf (db-one "SELECT COUNT(*) AS c FROM rules;") :|c|)))
    (when (= facts-count 0)
      ;; Minimal set of facts (tu peux en ajouter ensuite via l'UI)
      (db-exec "INSERT INTO facts(code,label) VALUES(?,?);" "F1" "PC lent")
      (db-exec "INSERT INTO facts(code,label) VALUES(?,?);" "F4" "Problème WiFi (ne se connecte pas)")
      (db-exec "INSERT INTO facts(code,label) VALUES(?,?);" "F10" "SMART disque: Attention/Mauvais")
      (db-exec "INSERT INTO facts(code,label) VALUES(?,?);" "F11" "Température CPU élevée")
      (db-exec "INSERT INTO facts(code,label) VALUES(?,?);" "F12" "Aérations obstruées / poussière")
      (db-exec "INSERT INTO facts(code,label) VALUES(?,?);" "F18" "Windows non à jour"))

    (when (= rules-count 0)
      ;; Rules (format simple) : conds_json = ["F1","F11","F12"] etc.
      ;; PC lent -> surchauffe -> action
      (db-exec "INSERT INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
               "R9"  "[\"F1\",\"F11\",\"F12\"]"
               "CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)")
      (db-exec "INSERT INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
               "R10" "[\"CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)\"]"
               "ACTION_RECOMMANDEE(pc_lent,Nettoyer le système de refroidissement)")

      ;; PC lent -> disque -> action
      (db-exec "INSERT INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
               "R7"  "[\"F1\",\"F10\"]"
               "CAUSE_POSSIBLE(pc_lent,disque_dur_degrade)")
      (db-exec "INSERT INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
               "R8"  "[\"CAUSE_POSSIBLE(pc_lent,disque_dur_degrade)\"]"
               "ACTION_RECOMMANDEE(pc_lent,Sauvegarder puis remplacer le disque (SSD))")

      ;; WiFi -> system not up to date -> action
      (db-exec "INSERT INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
               "R23" "[\"F4\",\"F18\"]"
               "CAUSE_POSSIBLE(wifi,systeme_non_a_jour)")
      (db-exec "INSERT INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
               "R24" "[\"CAUSE_POSSIBLE(wifi,systeme_non_a_jour)\"]"
               "ACTION_RECOMMANDEE(wifi,Installer les mises à jour puis redémarrer)"))))

(defun init-db ()
  (ensure-schema)
  (seed-if-empty))

;;; ----------------------------
;;; 4) Load rules/facts from DB
;;; ----------------------------

(defun load-facts ()
  ;; returns list of plists (:code "F1" :label "PC lent")
  (mapcar (lambda (row)
            (list :code (getf row :|code|) :label (getf row :|label|)))
          (db-all "SELECT code,label FROM facts ORDER BY code;")))

(defun load-rules ()
  ;; returns list of plists (:id "R9" :if (list ...) :then "...")
  (mapcar (lambda (row)
            (let* ((id (getf row :|id|))
                   (conds-json (getf row :|conds_json|))
                   (conds (cl-json:decode-json-from-string conds-json))
                   (then (getf row :|conclusion|)))
              (list :id id :if conds :then then)))
          (db-all "SELECT id,conds_json,conclusion FROM rules ORDER BY id;")))

;;; ----------------------------
;;; 5) Inference: forward chaining
;;; ----------------------------

(defun all-true-p (conds known)
  (every (lambda (c) (member c known :test #'string=)) conds))

(defun forward-chain (initial-facts)
  (let* ((rules (load-rules))
         (known (copy-list initial-facts))
         (trace '())
         (changed t))
    (loop while changed do
      (setf changed nil)
      (dolist (r rules)
        (let ((rid (getf r :id))
              (conds (getf r :if))
              (then (getf r :then)))
          (when (and (all-true-p conds known)
                     (not (member then known :test #'string=)))
            (push then known)
            (push (format nil "~A fired -> ~A" rid then) trace)
            (setf changed t)))))

    (let ((actions (remove-if-not
                    (lambda (f) (search "ACTION_RECOMMANDEE" f :test #'char=))
                    known)))
      (list :known (nreverse known)
            :actions (nreverse actions)
            :trace (nreverse trace)))))

;;; ----------------------------
;;; 6) Inference: backward chaining
;;; ----------------------------

(defun rules-that-produce (goal rules)
  (remove-if-not (lambda (r) (string= (getf r :then) goal)) rules))

(defun backward-prove-conditions (conds known rules visited)
  (let ((trace '()))
    (dolist (c conds)
      (multiple-value-bind (ok t2) (backward-prove c known rules visited)
        (setf trace (append trace t2))
        (unless ok
          (return-from backward-prove-conditions (values nil trace)))))
    (values t trace)))

(defun backward-prove (goal known rules &optional (visited '()))
  (cond
    ((member goal known :test #'string=)
     (values t (list (format nil "Goal already known: ~A" goal))))
    ((member goal visited :test #'string=)
     (values nil (list (format nil "Loop detected on: ~A" goal))))
    (t
     (let ((producers (rules-that-produce goal rules)))
       (if (null producers)
           (values nil (list (format nil "No rule produces: ~A" goal)))
           (dolist (r producers (values nil (list (format nil "All producers failed for: ~A" goal))))
             (let* ((rid (getf r :id))
                    (conds (getf r :if)))
               (multiple-value-bind (ok trace) (backward-prove-conditions conds known rules (cons goal visited))
                 (when ok
                   (return (values t
                                   (append (list (format nil "Use ~A to prove: ~A" rid goal))
                                           trace
                                           (list (format nil "Therefore proven: ~A" goal))))))))))))))

;;; ----------------------------
;;; 7) HTTP routes + static files
;;; ----------------------------

(define-easy-handler (healthz :uri "/healthz") ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "ok")

(define-easy-handler (api-facts :uri "/api/facts") ()
  (json-response (list :facts (load-facts))))

(define-easy-handler (api-rules :uri "/api/rules") ()
  ;; For debugging/demo display
  (let ((rules (load-rules)))
    (json-response (list :rules rules))))

(define-easy-handler (api-add-fact :uri "/api/facts" :method :post) ()
  (handler-case
      (let* ((p (read-json-body))
             (code (getf p :code))
             (label (getf p :label)))
        (cond
          ((or (null code) (null label) (string= code "") (string= label ""))
           (json-response (list :error "Missing code/label") :code 400))
          (t
           (db-exec "INSERT OR REPLACE INTO facts(code,label) VALUES(?,?);" code label)
           (json-response (list :ok t :code code :label label)))))
    (error (e) (json-response (list :error (princ-to-string e)) :code 400))))

(define-easy-handler (api-add-rule :uri "/api/rules" :method :post) ()
  ;; expects: { "id":"R99", "conds":["F1","F2"], "conclusion":"..." }
  (handler-case
      (let* ((p (read-json-body))
             (id (getf p :id))
             (conds (or (getf p :conds) '()))
             (conclusion (getf p :conclusion)))
        (cond
          ((or (null id) (string= id "") (null conclusion) (string= conclusion ""))
           (json-response (list :error "Missing id/conclusion") :code 400))
          ((not (listp conds))
           (json-response (list :error "conds must be a JSON array") :code 400))
          (t
           (let ((conds-json (cl-json:encode-json-to-string conds)))
             (db-exec "INSERT OR REPLACE INTO rules(id,conds_json,conclusion) VALUES(?,?,?);"
                      id conds-json conclusion)
             (json-response (list :ok t :id id))))))
    (error (e) (json-response (list :error (princ-to-string e)) :code 400))))

(define-easy-handler (api-diagnostic :uri "/api/diagnostic" :method :post) ()
  (handler-case
      (let* ((p (read-json-body))
             (facts (or (getf p :facts) '()))
             (mode  (or (getf p :mode) "forward"))
             (goal  (getf p :goal)))
        ;; log run (optional)
        (db-exec "INSERT INTO runs(mode,created_at,facts_json,goal) VALUES(?,?,?,?);"
                 mode (now-iso) (cl-json:encode-json-to-string facts) goal)

        (cond
          ((string= mode "forward")
           (json-response (forward-chain facts)))

          ((string= mode "backward")
           (let ((rules (load-rules)))
             (if (or (null goal) (string= goal ""))
                 (json-response (list :error "Missing goal for backward mode") :code 400)
                 (multiple-value-bind (ok trace) (backward-prove goal facts rules)
                   (json-response (list :goal goal :proven ok :trace trace))))))

          (t
           (json-response (list :error "mode must be forward/backward") :code 400))))
    (error (e)
      (json-response (list :error "Invalid request" :details (princ-to-string e)) :code 400))))

(defun setup-static-dispatch ()
  ;; Serve /static/* from /app/static
  (push (create-folder-dispatcher-and-handler "/static/" #p"/app/static/")
        *dispatch-table*)
  ;; Serve index.html at /
  (push (create-static-file-dispatcher-and-handler "/" #p"/app/static/index.html")
        *dispatch-table*))

;;; ----------------------------
;;; 8) Start server
;;; ----------------------------

(init-db)
(setup-static-dispatch)

(let* ((port (listen-port))
       (acc  (make-instance 'easy-acceptor :port port)))
  (format t "~&[expert] DB: ~A~%" *db-path*)
  (format t "~&[expert] Listening on port ~A~%" port)
  (start acc)
  (loop (sleep 3600)))
