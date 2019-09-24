;;; package --- Yauhen's emacs config
;;; Code:
;;; Commentary:

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(require 'cc-mode)
(setq
  package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("org" . "http://orgmode.org/elpa/")
                     ("melpa" . "http://melpa.org/packages/")
                     ("melpa-stable" . "http://stable.melpa.org/packages/"))
  package-archive-priorities '(("melpa-stable" . 1)))
 (package-initialize)
 
(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))
 
(defalias 'yes-or-no-p 'y-or-n-p)
 
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
                                        ;(toggle-frame-maximized)
(global-linum-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode)
(load-theme 'emacs-nw t t)
(enable-theme 'emacs-nw)
 
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(delete-selection-mode 1)

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))
 
(use-package helm
  :ensure t
  :demand
  :bind (
	 ("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("s-h" . helm-resume)
	 ("s-I" . helm-etags-select)
	 ("C-c F" . (lambda()
                      (interactive)
                      (helm-do-ag "/home/yauhen/ws")))
	 ("C-c f" . helm-do-ag)
	 )
  :config
  (setq helm-M-x-fuzzy-match t
	helm-apropos-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-completion-in-region-fuzzy-match t
	helm-etags-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-lisp-fuzzy-completion t
	helm-locate-fuzzy-match t
	helm-mode-fuzzy-match t
    helm-left-margin-width 100)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
  :after (helm projectile))
 
 (use-package highlight-symbol
   :bind
  ("C-x n" . highlight-symbol-next)
  ("C-x p" . highlight-symbol-prev)
  :config
  (highlight-symbol-mode t))

(use-package magit
  :ensure t
  :bind (("C-c g b" . magit-blame-addition)
	       ("C-c g s" . magit-status)
         ("C-c g l" . magit-log-buffer-file)))
(use-package browse-at-remote
  :bind (("C-c g r" . browse-at-remote)))

(use-package anzu
  :ensure t
  :bind (("C-c s" . anzu-query-replace-regexp)))

(use-package avy
  :ensure t
  :bind (("M-z" . avy-goto-word-or-subword-1)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c k" . ace-delete-window)))
 
(use-package flycheck
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
   :config
  ;(set-face-attribute 'flycheck-error nil :underline (list :color "red" :style 'wave) :foreground nil :background nil)
  ;(set-face-attribute 'flycheck-warning nil :underline (list :color "yellow" :style 'wave) :background nil :foreground nil))
  (global-flycheck-mode t))
 
(use-package flycheck-pos-tip
  :ensure t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
 
(use-package yasnippet
  :ensure t
  :demand
  :config
  (yas-minor-mode))
 
(use-package lsp-mode
  :ensure t
  :bind (
         ("C-c l d" . lsp-describe-thing-at-point)
         ("C-c l r" . lsp-rename)
         ("C-c l a" . lsp-execute-code-action)
         )
  :hook ((nxml-mode . lsp)
         (go-mode . lsp))
  :init
  (setq lsp-prefer-flymake nil))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package company
  :ensure t
  :config (global-company-mode t))
 
(use-package company-lsp
  :ensure t)
 
(use-package lsp-ui :ensure t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :after (lsp-mode))

(use-package lsp-java
  :ensure t
  :demand
  :after lsp
	:init
	(defvar yl/lsp-ui-doc-enabled nil)
	(defun yl/lsp-ui-doc-toggle ()
		"Toogles show of doc hover"
		(interactive)
		(if yl/lsp-ui-doc-enabled
				(lsp-ui-doc-show)
			(lsp-ui-doc-hide))
		(setq yl/lsp-ui-doc-enabled (not yl/lsp-ui-doc-enabled)))
	(require 'lsp-java-boot)
	(add-hook 'lsp-mode-hook #'lsp-lens-mode)
	(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  :bind (
         ("C-c l q" . yl/lsp-ui-doc-toggle)
         ("C-c l b" . lsp-java-build-project)
         ("C-c l n" . lsp-java-actionable-notifications)
         ("C-c l c" . lsp-java-update-project-configuration)
         ("C-c l g o" . lsp-java-generate-overrides)
         ("C-c l g g" . lsp-java-generate-getters-and-setters)
         ("C-c l g s" . lsp-java-generate-to-string)
         )
  :hook (java-mode . lsp))

;; (use-package golden-ratio
;;   :ensure t
;;   :config
;;   (setq golden-ratio-auto-scale nil
;;         golden-ratio-wide-adjust-factor 1
;;         golden-ratio-auto-scale 1)
;;   (define-advice select-window (:after (window &optional no-record) golden-ratio-resize-window)
;;     (golden-ratio)
;;     nil)
;;   (golden-ratio-mode -1))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))
 
(use-package dap-java :after (lsp-java))
 
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'light)
  (sml/setup))
 
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))
 
(use-package syntax-subword
  :ensure t
  :init (global-syntax-subword-mode)
  :config
  (defun forward-delete-word (arg)
    "Delete instead kill a word forward"
    (interactive "p")
    (delete-region (point) (progn (syntax-subword-forward arg) (point))))
  (defun backward-delete-word (arg)
    "Delete instead kill a word backward"
    (interactive "p")
    (delete-region (point) (progn (syntax-subword-backward arg) (point))))
  :bind (("C-<delete>" . forward-delete-word)
         ("C-<backspace>" . backward-delete-word)))

(use-package web-mode
  :ensure t
  :mode (("\\.html$"  . web-mode)
         ("\\.xhtml$" . web-mode)
         ("\\.tmpl$"  . web-mode)
         ("\\.tpl$"   . web-mode)
         ("\\.jsp$"   . web-mode)
         ("\\.less$"  . css-mode)
         )
  :init
  (setq web-mode-markup-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :mode (("\\.text$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :init
;;   (require 'smartparens-config)
;;   (show-smartparens-global-mode t)
;;   (smartparens-global-mode t)
;;   (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
;;   (sp-pair "\"" "\"" :actions '(wrap))
;;   (sp-pair "'" "'" :actions '(wrap)))
 
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
 
(use-package docker-compose-mode
  :ensure t)
 
(load-file "~/.emacs.d/utils/buffer.el")
 
(global-set-key (kbd "M-q") 'er/expand-region)
(global-set-key (kbd "s-?") 'xah-copy-file-path)
(global-set-key (kbd "s-n") 'xah-new-empty-buffer)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-c") "\C-a\C- \C-n\M-w\C-y\C-p") ; duplicate line
(global-set-key (kbd "C-x |") 'toggle-window-split)
 
(use-package ispell
  :config
  (add-to-list
   'ispell-hunspell-dict-paths-alist
   '("deutsch-hunspell" "/usr/share/myspell/de_DE.aff"))
  (add-to-list
   'ispell-hunspell-dict-paths-alist
   '("english-hunspell" "/usr/share/myspell/en_US.aff"))
  (setq ispell-program-name "hunspell"          ; Use hunspell to correct mistakes
        ispell-dictionary   "deutsch-hunspell") ; Default dictionary to use
  (defun switch-dictionary-de-en ()
    "Switch german and english dictionaries."
    (interactive)
    (let* ((dict ispell-current-dictionary)
           (new (if (string= dict "deutsch-hunspell") "english-hunspell"
                  "deutsch-hunspell")))
      (ispell-change-dictionary new)
      (message "Switched dictionary from %s to %s" dict new)))
  :bind (("C-c d" . switch-dictionary-de-en))
)

 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(highlight-symbol-face ((t (:background "gainsboro"))))
 '(linum ((t (:inherit (shadow default)))))
 '(lsp-ui-sideline-code-action ((t (:foreground "grey")))))
 
 (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-delay 0.3)
 '(golden-ratio-auto-scale t)
 '(golden-ratio-mode t)
 '(helm-use-frame-when-more-than-two-windows nil)
 '(yas-snippet-dirs (quote ("/home/yauhen/.emacs.d/snippets"))))
  '(highlight-symbol-idle-delay 0.3)
  '(package-selected-packages
    (quote
    (toggle-window docker-compose-mode lsp-java treemacs-magit treemacs-icons-dired treemacs-projectile treemacs web-mode syntax-subword smart-mode-line flycheck-gradle ace-window dap-mode company-lsp yasnippet yaml-mode yafolding xml+ x-path-walker web-beautify use-package tldr tidy thing-cmds sql-indent smartparens realgud rainbow-delimiters py-autopep8 org-mind-map move-text markdown-preview-mode magit lua-mode lsp-ui jtags json-reformat jedi javadoc-lookup imenus highlight-symbol highlight helm-projectile helm-ag golden-ratio go-rename go-impl go-guru go-eldoc go-complete go-autocomplete ggtags flycheck-pos-tip flycheck-plantuml expand-region easy-hugo drag-stuff dockerfile-mode direx-grep company-jedi company-go color-theme-modern browse-at-remote avy autodisass-java-bytecode auto-sudoedit anzu ac-helm)))
 '(projectile-mode t nil (projectile)))
  
(provide 'init)
;;; init.el ends here
