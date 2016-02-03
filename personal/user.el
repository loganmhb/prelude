;;; user.el -- customizations to Prelude

;;; Commentary:

;; Personalizations

;;; Code:

;; misc keybinding:

(defun join-lines-and-remove-whitespace ()
  (interactive)
  (join-line)
  (delete-trailing-whitespace))


(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-c j") 'join-lines-and-remove-whitespace)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "RET") 'newline-and-indent)

;; minimal UI

(menu-bar-mode -1)
(tool-bar-mode -1)

(when window-system
  (scroll-bar-mode -1)
  ;; stop making noise when I scroll
  (setq ring-bell-function #'ignore))


;; personal packages

(defvar my-packages '(olivetti clj-refactor rainbow-delimiters rust-mode
                               flycheck-rust flycheck-pos-tip
                               cider aggressive-indent))


(defun update-packages (packages)
  "Update custom packages defined in PACKAGES."
  (package-refresh-contents)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (condition-case err
          (package-install p)
        (error (message "%s" (error-message-string err)))))))


;; track the date of last package update so we don't have to update on every
;; restart:

(defun insert-current-date ()
  "Insert the current date into the buffer."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun write-date-to-disk ()
  "Write the current date to disk in a file."
  (with-temp-buffer
    (insert-current-date)
    (write-file "~/.emacs.d/.update.time")))

(defun get-string-from-file (file-path)
  "Return FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun update-packages-if-outdated ()
  "Iterate over the list of my-packages and update, once a day."
  (let ((current-date (substring (with-temp-buffer (insert-current-date)
                                                   (buffer-string))
                                 0 10))
        (last-updated (substring (get-string-from-file "~/.emacs.d/.update.time")
                                 0 10)))
    (if (not (string= current-date last-updated))
        (progn (update-packages my-packages)
               (write-date-to-disk)))))

(update-packages-if-outdated)

(defun create-shell-in-new-buffer ()
  "Create a new shell rather than switching to an open one."
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(global-set-key (kbd "C-c t") 'create-shell-in-new-buffer)



(add-to-list 'load-path "~/.emacs.d/personal/vendor")

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; cider customization

(add-hook 'cider-mode-hook #'eldoc-mode)


;; Custom indentation for clojure


(global-set-key (kbd "C-c h") 'whitespace-mode)


(defun switch-to-paredit ()
  "Turn off smartparens, turn on paredit."
  (smartparens-mode -1)
  (paredit-mode 1))


;; use clj-refactor
(require 'clj-refactor)

(add-hook 'clojure-mode-hook (lambda ()
                               (switch-to-paredit)
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-r")
                               (aggressive-indent-mode t)))


(add-hook 'lisp-mode-hook #'switch-to-paredit)
(add-hook 'emacs-lisp-mode-hook #'switch-to-paredit)


(mapc (lambda (s) (put-clojure-indent s 1))
      '(describe describe-server it
                 init-state render render-state will-mount did-mount should-update
                 will-receive-props will-update did-update display-name will-unmount
                 describe-with-db describe-with-server swaggered context around
                 legal-moves pseudo-legal-moves with facts fact
                 match describe-with-mock-etl-state describe-with-es))


(mapc (lambda (s) (put-clojure-indent s 2))
      '(GET* POST* PUT* DELETE* PATCH* context*))

;; rust

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; Org mode extensions

(setq org-babel-clojure-backend 'cider)

(require 'ob-clojure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (clojure . t)
   (plantuml . t)))

(add-to-list 'exec-path "~/bin")

(setq org-src-fontify-natively t)

(setq org-list-allow-alphabetical t)
(require 'ox-md)

(defun lmb-insert-org-src-block (lang)
  "Insert source block of language LANG."
  (interactive "sEnter source language: ")
  (insert "#+BEGIN_SRC " lang "\n\n#+END_SRC")
  (move-beginning-of-line 1)
  (forward-line -1))

;; Misc fns

(defun paste-from-osx ()
  "Paste from OS X."
  (shell-command-to-string "pbpaste"))

(defun copy-to-osx (text &optional push)
  "Copy TEXT so it is available to OS X. PUSH unused."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'copy-to-osx)
(setq interprogram-paste-function 'paste-from-osx)


(defun open-dot-emacs ()
  "Open this file."
  (interactive)
  (find-file "~/.emacs.d/personal/user.el"))


(defun note ()
  "Open notes.org."
  (interactive)
  (find-file "~/Dropbox/writing/notes.org"))


;; Writing settings

(setq prelude-whitespace nil)

(add-hook 'prelude-prog-mode-hook #'whitespace-mode)

(setq prelude-guru nil)

(add-hook 'text-mode-hook (lambda ()
                            (olivetti-mode)
                            (setq tab-width 4)))


(server-start)

(provide 'user)
;;; user.el ends here
