;;; package --- Yauhen's emacs config
;;; Code:
;;; Commentary:

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/Misc/emacs/go-mode.el/")
(add-to-list 'load-path "~/Misc/emacs/emacs-go-eldoc/")
(add-to-list 'load-path "~/Misc/emacs/flycheck-tip/")
(require 'go-mode-autoloads)

(add-hook 'before-save-hook 'gofmt-before-save)

(defvar init-GOPATH (getenv "GOPATH"))

(defun go-package-name(file)
	(replace-regexp-in-string (concat init-GOPATH "/src/") "" file))

(defun go-test-package()
	(interactive)
	(shell-command "gb test -v"))

(defun run-in-project(command)
	(interactive "s")
	(projectile-with-default-dir (projectile-project-root)
		(shell-command command)))

(defun gb-build-project()
	(interactive)
	(run-in-project "gb build"))

(defun goose-up-project()
	(interactive)
	(run-in-project "goose up"))

(defun goose-down-project()
	(interactive)
	(run-in-project "goose down"))

(defun go-run-file()
	(interactive)
	(shell-command (concat "go run " (buffer-file-name))))

(defun go-local-playground()
	(interactive)
	(let ((tmp-go-file (concat "/home/yauhen/ws/src/playground/"
														 (replace-regexp-in-string "\\." "playground\/" (number-to-string (float-time)))
														 ".go")))
		(let ((dir (file-name-directory tmp-go-file)))
			(make-directory dir)
			(copy-file "~/.emacs.d/gotmpl.go" tmp-go-file)
			(find-file tmp-go-file))))

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
													(local-set-key (kbd "C-c o") 'godoc-at-point)
													(local-set-key (kbd "C-c r") 'go-run-file)
													(local-set-key (kbd "C-c t") 'go-test-package)
                          (highlight-symbol-mode t)
                          (local-set-key (kbd "s-.") 'highlight-symbol-next)
                          (local-set-key (kbd "s-,") 'highlight-symbol-prev)
													))

(global-set-key (kbd "C-c g u") 'goose-up-project)
(global-set-key (kbd "C-c g d") 'goose-down-project)
(global-set-key (kbd "s-b") 'gb-build-project)
(global-set-key (kbd "s-p") 'go-local-playground)
(global-set-key (kbd "s-`") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c b") 'magit-blame)

(global-set-key (kbd "C-c F") '(lambda()
																	 (interactive)
																	 (helm-do-ag (concat (getenv "JUNO") "/"))))

(global-set-key (kbd "C-c f") 'helm-do-ag)

(add-to-list 'load-path "$GOPATH/src/github.com/nsf/gocode/emacs/")

(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
(load-file "$GOPATH/src/golang.org/x/tools/refactor/rename/go-rename.el")

(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
;(require 'flycheck-tip)
;(flycheck-tip-use-timer 'verbose)
(set-face-attribute 'eldoc-highlight-function-argument nil :underline nil :foreground "red" :weight 'bold)

(require 'flycheck)
(flycheck-define-checker gb-build
  "A Go syntax and type checker using the `gb build' command."
  :command ("gb" "build")
  :error-patterns ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes go-mode)

(add-to-list 'flycheck-checkers 'gb-build)
(setq-default flycheck-disabled-checkers '(go-vet go-golint go-build))
(set-face-attribute 'flycheck-error nil :underline (list :color "red" :style 'wave) :foreground nil :background nil)
(set-face-attribute 'flycheck-warning nil :underline (list :color "yellow" :style 'wave) :background nil :foreground nil)

(ac-config-default)

;(require 'sr-speedbar)

(linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
;;(scroll-bar-mode -1)

(setq tab-width 2)
(setq default-tab-width 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(flycheck-display-errors-delay 1.0)
 '(flycheck-go-build-executable "gb")
 '(git-commit-summary-max-length 256)
 '(highlight-symbol-idle-delay 0.3)
 '(inhibit-startup-screen t)
 '(json-reformat:indent-width 2)
 '(json-reformat:pretty-string\? t)
 '(semantic-mode t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(sql-indent-maybe-tab t)
 '(sql-indent-offset 2)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-width 20 t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 121 :width normal)))))

(toggle-frame-maximized)

;(desktop-save-mode 1)

(autoload 'mark-thing "thing-cmds")
(defun mark-a-word-or-thing (arg)
    "Select word on or before current point, and move point to beginning of word.
    
    With a prefix ARG, first prompts for type of things and select ARG things
    but you need to move the point to the beginnig of thing first.
    
    But if a thing has been selected, then extend the selection by one thing
    on the other side of the point.
    (So to select backwards, select to the right first.)"
      (interactive "P")
      (if (or arg mark-active)
        (call-interactively 'mark-thing)
        (skip-syntax-backward "w_")
        (mark-thing 'word)))

(global-set-key (kbd "s-w") 'mark-a-word-or-thing)

(global-set-key (kbd "C-c C-c") "\C-a\C- \C-n\M-w\C-y")
(global-set-key (kbd "s-l") (kbd "C-c ! l"))
(global-set-key (kbd "<s-down>") (kbd "C-c ! n"))
(global-set-key (kbd "<s-up>") (kbd "C-c ! p"))
;;Move line up
(global-set-key (kbd "<M-up>") 'move-text-up)
;;Move line down
(global-set-key (kbd "<M-down>") 'move-text-down)

(delete-selection-mode 1)

(projectile-global-mode)
;(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)


(add-hook 'json-mode 'flymake-json-load)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)

(setq make-backup-files nil)
(setq auto-save-default nil)

(package-initialize)
(elpy-enable)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun close-all-buffers ()
	"Closes all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "s-?") 'show-file-name)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-f j") 'json-reformat-region)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-h") 'helm-resume)
(global-set-key (kbd "s-<f5>") 'revert-buffer)
(global-set-key (kbd "C-x C-k") 'close-all-buffers)

(require 'golden-ratio)
(golden-ratio-mode 1)

;(global-set-key (kbd "S-s-<left>") '(lambda ()
;				      (interactive)
;				      (shrink-window-horizontally 10)))
;(global-set-key (kbd "S-s-<right>") '(lambda ()
;				       (interactive)
;				       (enlarge-window-horizontally 10)))
;(global-set-key (kbd "S-s-<down>") 'shrink-window)
;(global-set-key (kbd "S-s-<up>") 'enlarge-window)


(add-to-list 'yas-snippet-dirs "~/Misc/emacs/yasnippet-go")

(require 'yasnippet)
(yas-global-mode 1)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ubuf (generate-new-buffer "untitled")))
    (switch-to-buffer ubuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(global-set-key (kbd "s-n") 'xah-new-empty-buffer)
(global-set-key (kbd "s-<f2>") 'rename-file-and-buffer)

(global-set-key (kbd "s-I") 'utl-imenus-search-project-go-files)
(global-set-key (kbd "s-i") 'imenus)

(require 'find-lisp)
(require 'imenus)

(defun utl-imenus-search-project-go-files ()
  "Perform `imenus' on Go files from `projectile-root-dir'."
  (interactive)
  (let (
	(files (delq nil (mapcar (lambda (filename) (if (string= "go" (file-name-extension filename)) filename nil)) (projectile-current-project-files)))))
    (imenus-files files nil "Search in project:")))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml.tmpl$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml.tmpl$" . yaml-mode))

(setq-default indent-tabs-mode nil)

(defun lower-and-concat (b e)
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (downcase-region b e)
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match "_"))))

(global-set-key (kbd "s-u") 'lower-and-concat)
(global-set-key (kbd "s-q") 'er/expand-region)

(provide 'init)
;;; init.el ends here
