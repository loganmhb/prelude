;;; user.el -- customizations to Prelude

;;; Commentary:

;; Personalizations

;;; Code:

;; misc keybinding:


(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "RET") 'newline-and-indent)

;; minimal UI

(menu-bar-mode -1)
(tool-bar-mode -1)

;; turn off Flycheck

(flycheck-mode -1)

;; stop making noise when I scroll

(setq ring-bell-function #'ignore)

;; personal packages

(defvar my-packages '(olivetti clj-refactor rainbow-delimiters rust-mode
                      flycheck-rust flycheck-clojure flycheck-pos-tip))


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

;; (mapc (lambda (s) (put-clojure-indent s 1))
;;      '(describe describe-server it before-all after-all before after
;;                 init-state render render-state will-mount did-mount should-update
;;                 will-receive-props will-update did-update display-name will-unmount
;;                 describe-with-db describe-with-server swaggered))

;; (mapc (lambda (s) (put-clojure-indent s 'defun))
;;      '(GET* PUT* DELETE* POST* PATCH* context))


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
                               (cljr-add-keybindings-with-prefix "C-c C-r")))


(add-hook 'lisp-mode-hook #'switch-to-paredit)
(add-hook 'emacs-lisp-mode-hook #'switch-to-paredit)


(mapc (lambda (s) (put-clojure-indent s 1))
      '(describe describe-server it before-all after-all before after
                 init-state render render-state will-mount did-mount should-update
                 will-receive-props will-update did-update display-name will-unmount
                 describe-with-db describe-with-server swaggered context around
                 legal-moves pseudo-legal-moves GET* POST* PUT* PATCH* DELETE*))

;; flycheck for clojure

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; rust

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


(defun pretty-lambdas-haskell ()
  "Replace \ with lambda in haskell mode."
  (font-lock-add-keywords
   nil `((,(concat "\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

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

(defun json->clj-map ()
  "Reformat a JSON string as a Clojure map."
  (interactive)
  (when (region-active-p)
    (replace-regexp "null" "nil"
                    nil (region-beginning) (region-end))
    (replace-regexp "\\(\"\\([A-z0-9_-]+\\)\"\s*:\\)" ":\\2 "
                    nil (region-beginning) (region-end))
    (replace-regexp "," ""
                    nil (region-beginning) (region-end))))



(defun open-dot-emacs ()
  "Open this file."
  (interactive)
  (find-file "~/.emacs.d/personal/user.el"))


;; Writing settings

(setq prelude-whitespace nil)

(add-hook 'prelude-prog-mode-hook #'whitespace-mode)

(setq prelude-guru nil)

(add-hook 'text-mode-hook (lambda ()
                            (olivetti-mode)
                            (setq tab-width 4)))


(provide 'user)
;;; user.el ends here
