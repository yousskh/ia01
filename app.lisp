;;;; app.lisp
;;;; Minimal Common Lisp API for an expert system demo (Render + Docker)

(ql:quickload '(:hunchentoot :cl-json))

(defpackage :expert-demo
  (:use :cl :hunchentoot))
(in-package :expert-demo)

;; --- Helpers ------------------------------------------------------------

(defun env (name &optional default)
  (or (uiop:getenv name) default))

(defun listen-port ()
  (parse-integer (env "PORT" "10000") :junk-allowed t))

(defun json-response (obj &key (code 200))
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (setf (hunchentoot:return-code*) code)
  (cl-json:encode-json-to-string obj))

(defun read-json-body ()
  (let* ((raw (hunchentoot:raw-post-data :force-text t)))
    (when (and raw (> (length raw) 0))
      (cl-json:decode-json-from-string raw))))

;; --- Knowledge base (very small stub) ----------------------------------
;; Tu remplaceras ça par ta base de faits/règles complète.

(defun forward-chain (facts)
  "Retourne une trace + conclusions (exemple)."
  ;; Exemple: si facts contient 'F1 'F11 'F12 -> surchauffe -> action nettoyage
  (let ((trace '())
        (conclusions '())
        (actions '()))
    (when (and (member "F1" facts :test #'string=)
               (member "F11" facts :test #'string=)
               (member "F12" facts :test #'string=))
      (push "R9 fired: F1+F11+F12 -> CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)" trace)
      (push "CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)" conclusions)
      (push "R10 fired: CAUSE_POSSIBLE(...) -> ACTION_RECOMMANDEE(...,nettoyage_refroidissement)" trace)
      (push "ACTION_RECOMMANDEE(pc_lent,Nettoyer le système de refroidissement)" actions))
    (list :trace (nreverse trace)
          :conclusions (nreverse conclusions)
          :actions (nreverse actions))))

(defun backward-chain (goal facts)
  "Essaie de prouver un goal (exemple)."
  ;; Exemple de but : "ACTION_RECOMMANDEE(pc_lent,Nettoyer le système de refroidissement)"
  (let ((trace '()))
    (cond
      ((string= goal "ACTION_RECOMMANDEE(pc_lent,Nettoyer le système de refroidissement)")
       (push "Goal matches R10 conclusion, need CAUSE_POSSIBLE(pc_lent,surchauffe_thermique)" trace)
       (if (and (member "F1" facts :test #'string=)
                (member "F11" facts :test #'string=)
                (member "F12" facts :test #'string=))
           (progn
             (push "R9 supports needed cause; facts F1,F11,F12 are true -> goal proven" trace)
             (list :proven t :trace (nreverse trace)))
           (progn
             (push "Missing facts for R9 (need F1,F11,F12) -> goal not proven" trace)
             (list :proven nil :trace (nreverse trace)))))
      (t
       (list :proven nil :trace (list "Unknown goal in demo stub"))))))

;; --- HTTP Routes --------------------------------------------------------

(define-easy-handler (healthz :uri "/healthz") ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  "ok")

(define-easy-handler (diagnostic :uri "/api/diagnostic" :method :post) ()
  (handler-case
      (let* ((payload (read-json-body))
             (facts (or (getf payload :facts) '()))
             (mode  (or (getf payload :mode) "forward"))
             (goal  (getf payload :goal)))
        (cond
          ((string= mode "forward")
           (json-response (forward-chain facts)))
          ((string= mode "backward")
           (json-response (backward-chain (or goal "") facts)))
          (t
           (json-response (list :error "mode must be 'forward' or 'backward'") :code 400))))
    (error (e)
      (json-response (list :error "invalid request" :details (princ-to-string e)) :code 400))))

;; --- Start server -------------------------------------------------------

(let* ((port (listen-port))
       (acceptor (make-instance 'easy-acceptor :port port)))
  (format t "~&Starting expert-demo on port ~A~%" port)
  (start acceptor)
  ;; keep process alive
  (loop (sleep 3600)))
