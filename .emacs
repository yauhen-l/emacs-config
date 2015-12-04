(defadvice rgrep (around rgrep-init)
	"Init grep defaults before calling rgrep non-interactively."
	(when (not (called-interactively-p))
		(grep-compute-defaults))
	ad-do-it)

(ad-activate 'rgrep)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(add-to-list 'load-path "~/Misc/emacs/go-mode.el/")
(require 'go-mode-autoloads)

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
													(local-set-key (kbd "C-c o") 'godoc-at-point)))

(defun search-in-project (regexp)
	(interactive "s")
	(rgrep regexp "*.go" "~/ws/src/junolab.net/"))
(global-set-key (kbd "C-c C-g") 'search-in-project)

(defun ag-juno ()
	(interactive)
	(helm-do-ag "~/ws/src/junolab.net/"))
(global-set-key (kbd "C-c C-f") 'ag-juno)
(global-set-key (kbd "C-c f") 'helm-do-ag)

(add-to-list 'load-path "~/ws/src/github.com/dougm/goflymake/")
(add-to-list 'load-path "~/ws/src/github.com/nsf/gocode/emacs/")
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))

(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
(load-file "$GOPATH/src/golang.org/x/tools/refactor/rename/go-rename.el")

(require 'go-flymake)
(require 'go-flycheck)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'golint)
(ac-config-default)

(load-file "/home/yauhen/ws/other/python-yapf.el/python-yapf.el")
(require 'python-yapf)

(require 'sr-speedbar)

(linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-tab-width 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(inhibit-startup-screen t)
 '(json-reformat:indent-width 2)
 '(json-reformat:pretty-string\? t)
 '(semantic-mode t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-width 20 t)
 '(tool-bar-mode nil))

;(sr-speedbar-open)
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

(global-set-key (kbd "C-c C-w") 'mark-a-word-or-thing)

(global-set-key "\C-c\C-c" "\C-a\C- \C-n\M-w\C-y")
;;Move line up
(global-set-key (kbd "<M-up>") 'move-text-up)
;;Move line down
(global-set-key (kbd "<M-down>") 'move-text-down)

(delete-selection-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'helm-config)
(helm-mode 1)

(require 'projectile-speedbar)
(global-set-key (kbd "M-<f3>") 'projectile-speedbar-open-current-buffer-in-tree)

(add-hook 'json-mode 'flymake-json-load)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)

(setq make-backup-files nil)
(setq auto-save-default nil)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key (kbd "s-?") 'show-file-name)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-f j") 'json-reformat-region)
(global-set-key (kbd "s-i") 'imenu)
(global-set-key (kbd "s-g") 'magit-status)

(global-set-key (kbd "C-x r") 'revert-buffer)

;(setq frame-title-format
;      (list (format "%s %%S: %%j " (system-name))
;						'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


(setq pycodechecker "pychecker")

(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(add-hook 'python-mode-hook 'flymake-mode)


(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun go-oracle-set-scope-to-gopath ()
  "Set go-oracle scope to GOPATH/src/**/*."
  (interactive)
  (go-projectile-set-gopath)
  (setq go-oracle-scope (file-expand-wildcards (concat (getenv "GOPATH") "/src/**/*"))))

(defun nil-message ()
	"Set minibuffer to home"
	(interactive)
	(message nil))

(global-set-key (kbd "s-h") 'nil-message)
	

;;(require 'golden-ratio)
;;(golden-ratio-mode 1)
