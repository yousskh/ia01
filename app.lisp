;;;; app.lisp — Expert system (Common Lisp + SQLite CLI + Web UI)

;; Load Quicklisp
(load "/root/quicklisp/setup.lisp")
(ql:quickload '(:hunchentoot :cl-json :uiop))

(defpackage :expert
  (:use :cl :hunchentoot))
(in-package :expert)

;; CONFIG
(defparameter *db-path* "/app/data/expert.db")

(defun env (name &optional (default "10000"))
  (or (uiop:getenv name) default))

(defun port ()
  (parse-integer (env "PORT" "10000")))

;; SQLITE HELPERS
(defun sql (query)
  (uiop:run-program
   (list "sqlite3" *db-path* query)
   :output :lines))

(defun sql-exec (query)
  (uiop:run-program
   (list "sqlite3" *db-path* query)
   :output :string))

;; LOAD DB-INIT.LISP
(load "/app/db-init.lisp")

;; LOAD FACTS / RULES
(defun load-facts ()
  (mapcar
   (lambda (l)
     (destructuring-bind (code label category) (uiop:split-string l :separator "|")
       (list :code code :label label :category category)))
   (sql "SELECT code||'|'||label||'|'||category FROM facts;")))

(defun load-rules ()
  (mapcar
   (lambda (l)
     (destructuring-bind (id conds then action-done) (uiop:split-string l :separator "|")
       (list :id id :conds (uiop:split-string conds :separator ";") :then then :action-done (if (string= action-done "") nil action-done))))
   (sql "SELECT id||'|'||conds||'|'||conclusion||'|'||COALESCE(action_done,'') FROM rules;")))

;; FORWARD CHAINING
(defun forward-chain (facts)
  (let ((known (copy-list facts)) (trace '()) (rules (load-rules)) (changed t))
    (loop while changed do
      (setf changed nil)
      (dolist (r rules)
        (when (and (every (lambda (c) (member c known :test #'string=)) (getf r :conds))
                   (not (member (getf r :then) known :test #'string=)))
          (push (getf r :then) known)
          (push (format nil "~A fired -> ~A" (getf r :id) (getf r :then)) trace)
          (setf changed t))))
    (list :facts known :trace (nreverse trace) :actions (remove-if-not (lambda (f) (search "ACTION_RECOMMANDEE" f)) known))))

;; BACKWARD CHAINING
;; Trouve les problèmes potentiellement résolus par les actions effectuées
(defun backward-chain (selected-actions)
  (let ((rules (load-rules))
        (resolved-problems '())
        (trace '()))
    ;; Pour chaque action sélectionnée, trouver les règles qui la produisent
    (dolist (action selected-actions)
      ;; Trouver la règle qui conclut cette action
      (dolist (r rules)
        (when (string= (getf r :then) action)
          ;; Trouver les conditions de cette règle (les CAUSE_POSSIBLE)
          (dolist (cond (getf r :conds))
            ;; Pour chaque CAUSE_POSSIBLE, trouver les faits qui y mènent
            (dolist (r2 rules)
              (when (string= (getf r2 :then) cond)
                ;; Les conditions de r2 sont les faits/symptômes
                (dolist (fact-code (getf r2 :conds))
                  (unless (search "CAUSE_POSSIBLE" fact-code)
                    (push fact-code resolved-problems)
                    (push (format nil "~A peut résoudre le problème ~A" action fact-code) trace)))))))))
    ;; Dédupliquer les problèmes résolus
    (list :resolved (remove-duplicates resolved-problems :test #'string=) 
          :trace (nreverse trace))))

;; Récupérer les labels des faits par leurs codes
(defun get-fact-labels (fact-codes)
  (let ((facts (load-facts))
        (result '()))
    (dolist (code fact-codes)
      (dolist (f facts)
        (when (string= (getf f :code) code)
          (push (list :code code :label (getf f :label) :category (getf f :category)) result))))
    (nreverse result)))

;; HTTP API
(defun json (obj)
  (setf (content-type*) "application/json")
  (with-output-to-string (s)
    (cl-json:encode-json obj s)))

(define-easy-handler (health :uri "/healthz") ()
  "ok")

(define-easy-handler (api-facts :uri "/api/facts") ()
  (let ((facts (load-facts))
        (result-array '()))
    (dolist (f facts)
      (push (list (getf f :code) (getf f :label) (getf f :category)) result-array))
    (let ((json-obj (make-hash-table :test 'equal)))
      (setf (gethash "facts" json-obj) (nreverse result-array))
      (json json-obj))))

;; Endpoint pour récupérer toutes les actions disponibles (pour le chaînage arrière)
(define-easy-handler (api-actions :uri "/api/actions") ()
  (let ((rules (load-rules))
        (seen (make-hash-table :test 'equal))
        (result-array '()))
    ;; Collecter toutes les règles avec ACTION_RECOMMANDEE et action-done
    (dolist (r rules)
      (let ((conclusion (getf r :then))
            (action-done (getf r :action-done)))
        (when (and (search "ACTION_RECOMMANDEE" conclusion)
                   action-done
                   (not (gethash conclusion seen)))
          (setf (gethash conclusion seen) t)
          ;; Extraire le type depuis la conclusion
          (let* ((start (+ (search "(" conclusion) 1))
                 (end (1- (length conclusion)))
                 (content (subseq conclusion start end))
                 (comma-pos (position #\, content)))
            (when comma-pos
              (let ((action-type (subseq content 0 comma-pos)))
                (push (list conclusion action-type action-done) result-array)))))))
    (let ((json-obj (make-hash-table :test 'equal)))
      (setf (gethash "actions" json-obj) (nreverse result-array))
      (json json-obj))))

(define-easy-handler (api-run :uri "/api/diagnostic") ()
  (let* ((raw-json (raw-post-data :force-text t))
         (p (cl-json:decode-json-from-string raw-json))
         (facts (cdr (assoc :facts p)))
         (mode (cdr (assoc :mode p)))
         (selected-actions (cdr (assoc :selected-actions p)))
         (json-obj (make-hash-table :test 'equal)))
    (if (string= mode "backward")
        ;; Chaînage arrière : trouver les problèmes résolus par les actions
        (let* ((result (backward-chain selected-actions))
               (resolved-codes (getf result :resolved))
               (resolved-facts (get-fact-labels resolved-codes)))
          (setf (gethash "mode" json-obj) "backward")
          (setf (gethash "resolved_problems" json-obj) 
                (mapcar (lambda (f) 
                          (list (getf f :code) (getf f :label) (getf f :category))) 
                        resolved-facts))
          (setf (gethash "trace" json-obj) (getf result :trace)))
        ;; Chaînage avant : diagnostic classique
        (let ((result (forward-chain facts)))
          (setf (gethash "mode" json-obj) "forward")
          (setf (gethash "inference_result" json-obj) (getf result :trace))
          (setf (gethash "rule_conclusions" json-obj) (getf result :actions))))
    (json json-obj)))

;; STATIC FILES
(push (create-folder-dispatcher-and-handler "/static/" #p"/app/static/") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/" #p"/app/static/index.html") *dispatch-table*)

;; START SERVER
(format t "~&Initializing database...~%")
(init-db)
(format t "~&Starting server on port ~A...~%" (port))
(start (make-instance 'easy-acceptor :port (port)))
(format t "~&Server running. Press Ctrl+C to exit.~%")
(loop (sleep 3600))