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


;;; run

(setq sshlist (ssh-config-parse))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
      str)

(defun get-remote-home (USER)
  "Return home directory of USER. Works with tramp sessions too,
because `shell-command` is being used which is `default-directory`
agnostic. That is, if the current buffer (where the user hit
C-x C-s) is a tramp buffer, then `shell-command`will be executed
remotely via ssh."
  (let ((home nil)
        (shell "*Shell Command Output*"))
      (with-current-buffer shell
        (shell-command (format "ls -d ~%s" USER))
        (setq home (chomp (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer shell)
      home))

;; This advice will be executed  everytime emacs creates a backup.  it
;; determines if  the file to  be backed up  is remote, finds  out the
;; user it belongs to, and fixes the backup directory permissions.
;; Finally it sets `backup-directory-alist appropriately.
;;
;; FIXME: test this crab!
(advice-add
 'make-backup-file-name-1
 :before
 '(lambda (&rest file)
    (let ((filename (car file))
          (username nil)
          (hostname nil)
          (homedir nil)
          (sshentry nil))
      (cond
       ((string-match "/ssh:\\(.+\\)@\\(.+\\):/" filename)
        (setq username (match-string 1 filename))
        (setq hostname (match-string 2 filename)))
       ((string-match "/ssh:\\(.+\\):/" filename)
        (setq hostname (match-string 1 filename))))
      (if (not hostname)
          ;; LOCAL backup, stick with default
          (setq backup-directory-alist `(("." . ,my-backup-directory)))
        ;; SSH backup, fix and customize
        (setq sshentry (ssh-match sshlist hostname))
        (if (not sshentry)
            ;; not configured, use parsed results
            (if (not username)
                (setq username (user-login-name)))
          ;; sshentry is filled
          (if (not username)
              ;; use user from config, else, use parsed one
              (setq username (car sshentry))))
        ;; now set up permissions and add the dir to backupdirs
        (setq homedir (get-remote-home username))
        ;; FIXME: when is the mkdir being executed? Maybe do it here before the chmod!
        (shell-command (format "chmod 700 %s/.emacs.d/backups" homedir))
        (setq backup-directory-alist `(("." . ,(format ":%s/.emacs.d/backups" homedir))))))))
