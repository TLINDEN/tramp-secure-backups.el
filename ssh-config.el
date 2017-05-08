;; usage:

;; parse config into alist 'ssh:
;; (setq ssh (ssh-config-parse))
;;
;; =>  (("\\(klingel\\)" "root" "192.168.128.108")
;;     ("\\(bsdpi\\)"   "madi" "192.168.128.19"))

;; return matching user+hostname pair for bb211 (maybe a wildcard entry)
;; (ssh-match ssh "bb211")
;;
;; => ("cisco" "bb211")

;; .ssh/config entry for this:
;;    Host bb*
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
          (when host
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
      (when host ; check again, in case some host left unlogged
        (when (not user)
          (setq user current-user))
        (add-to-list 'ssh (list (glob-to-regex host) user hostname) t)))
    ssh))

(defun ssh-get-assoc(H R)
  "Return hostname,username from alist H matching regexp R"
  (cdr (assoc R H)))

(defun ssh-match (H N)
  "Return username,hostname from hostname N matching key(H) in alist H.

If entry has no hostname, N is returned, if it has no username, current user
will be returned. Returned value is a list. Access with car and cdr."
  (catch 'break (dolist (cell H)
                  (let* ((R (car cell))
                         (U (car (ssh-get-assoc H R)))
                         (H (or (car (cdr (ssh-get-assoc H R))) N)))
                    (when (string-match R N)
                      (throw 'break (list U H)))))))
