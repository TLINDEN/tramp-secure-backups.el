;;; usage:

;;;; parse config into alist 'ssh:
;;
;; (setq ssh (ssh-config-parse))
;;
;; =>  (("\\(klingel\\)" "root" "192.168.128.108")
;;     ("\\(bsdpi\\)"   "madi" "192.168.128.19"))

;;;; return matching user+hostname pair for bb211 (maybe a wildcard entry):
;;
;; (ssh-match ssh "bb211")
;;
;; => ("cisco" "bb211")

;;;; print:
;;
;; (message (format "%s@%s" (car (ssh-match ssh "bb211"))
;;                          (cdr (ssh-match ssh "bb311"))))
;;
;; => "cisco@bb311"

;;;; .ssh/config entry for this:
;;
;; => Host bb*
;;      User cisco


(defun glob-to-regex (glob)
  "Return regexified version of GLOB surrounded by matching parens."
  (concat "\\("
          (replace-regexp-in-string "*" ".*" (replace-regexp-in-string "?" "." glob))
          "\\)"))

(defun get-line ()
  "Return current line from begin to end w/o properties."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun ssh-add-entry (sshlist host hostname user)
  (when (and host (or user hostname))
    (when (not user)
      (setq user current-user))
    (add-to-list 'sshlist (list (glob-to-regex host) user hostname) t)))

(defun ssh-config-parse ()
  "Parse ~/.ssh/config and return content as alist."
  (interactive)
  (setq case-fold-search t)
  (let ((config (expand-file-name "~/.ssh/config"))
        (line nil)
        (host nil)
        (hostname nil)
        (user nil)
        (current-user (user-login-name))
        (ssh ()))
    (with-temp-buffer
      (insert-file-contents config)
      (goto-char 1)
      (while (not (eobp))
        (setq line (get-line))
        (when (string-match "^Host \\(.+\\)" line)
          (when (and host (or user hostname))
            (when (not user)
              (setq user current-user))
            (add-to-list 'ssh (list (glob-to-regex host) user hostname) t))
          (setq user nil
                hostname nil
                host (match-string 1 line)))
        (when (string-match "\s*User \\(.+\\)" line)
          (setq user (match-string 1 line)))
        (when (string-match "\s*Hostname \\(.+\\)" line)
          (setq hostname (match-string 1 line)))
        (next-line))
      (when (and host (or user hostname)); check again, in case some host left unlogged
        (when (not user)
          (setq user current-user))
        (add-to-list 'ssh (list (glob-to-regex host) user hostname) t)))
    ssh))

(defun ssh-match (sshlist N)
  "Return username,hostname from hostname N matching key(sshlist) in alist H.

If entry has no hostname, N is returned, if it has no username, current user
will be returned. Returned value is a cons cell. Access with car and cdr."
  (catch 'break (dolist (cell sshlist)        ; ("\\(dpl\\)" "root" "dipol.foo.bar") 
                  (let* ((R (car cell))       ; "\\(dpl\\)"
                         (P (cdr cell))       ; ("root" . "dipol.foo.bar")
                         (U (car P))          ; "root"
                         (H (car (cdr P))))   ; "dipol.foo.bar"
                    (when (string-match R N)
                      (message (format "R: %s, P: %s, U: %s, H: %s" R P U H))
                      (throw 'break (cons U (or H N))))))))


