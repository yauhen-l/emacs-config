;;; package --- Yauhen's emacs config
;;; Code:
;;; Commentary:


(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(require 'cc-mode)
(setq
  package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("melpa" . "http://melpa.org/packages/"))
  package-archive-priorities '(("melpa" . 1))
  package-install-upgrade-built-in t)
(package-initialize)

;(use-package org
; :pin gnu)

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
;(global-linum-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode)
(load-theme 'emacs-nw t t)
(enable-theme 'emacs-nw)
(pixel-scroll-precision-mode 1)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(delete-selection-mode 1)
(setq enable-recursive-minibuffers t)
(setq package-native-compile t)

(global-set-key (kbd "C-c l") #'dictionary-lookup-definition)
(setq dictionary-server "dict.org")

;; (use-package auto-package-update
;;    :ensure t
;;    :config
;;    (setq auto-package-update-delete-old-versions t
;;          auto-package-update-interval 4)
;;    (auto-package-update-maybe))

(use-package auto-compile
  :ensure t
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

; (use-package envrc
;   :ensure t
;   :config
;   (envrc-global-mode))

(use-package crux
  :ensure t
  :bind (("C-c D" . crux-delete-file-and-buffer)
         ("C-c N" . crux-cleanup-buffer-or-region)
         ("C-c O" . crux-open-with)
         ("C-c C-d" . crux-duplicate-current-line-or-region)
         ("C-c S" . crux-find-shell-init-file))
  )
(defun my/crux-reopen-as-root () "Reopen file as root." (interactive) (crux-reopen-as-root))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-search-path '("~/ws" "~/ws/raisin"
                                         "~/ws/raisin/ffb"
                                         "~/ws/ffb"
                                         "~/ws/deuba"
                                         "~/ws/devops"
                                         "~/.emacs.d"
                                         "~/ws-private/"))
  ;(setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-switch-project-action 'helm-projectile)
  (add-hook 'projectile-after-switch-project-hook #'envrc-reload)
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
        helm-left-margin-width 0)
  (helm-mode 1))

(use-package helm-file-preview
  :ensure t
  :config
  (helm-file-preview-mode 1))

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
  :config (
           (magit-add-section-hook 'magit-status-sections-hook
                        #'magit-insert-worktrees
                        nil t)
           )
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
  (yas-global-mode 1))

(use-package lsp-mode
  :ensure t
  :bind (
         ("C-c j d" . lsp-describe-thing-at-point)
         ("C-c j r" . lsp-rename)
         ("C-c j a" . lsp-execute-code-action)
         ("C-c j i" . lsp-goto-implementation)
         ("C-c j h" . lsp-treemacs-call-hierarchy)
         )
  :config
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/usr/local/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          "/usr/local/lib/node_modules"
          "--stdio"))
  :hook ((typescript-mode . lsp)
         (nxml-mode . lsp)
         (java-mode . lsp)
         (go-mode . lsp))
  :init
  (setq lsp-prefer-flymake nil))

(use-package treemacs
  :ensure t)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol
  :bind (("C-c j s" . helm-lsp-workspace-symbol)))

(use-package company
  :ensure t
  :config (global-company-mode t))

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
  :config
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
  (setq lsp-disabled-clients '(tfls))

  (require 'projectile)
  (require 'files)
  (defun yl/determine-jdt-workspace (base-path)
    "Find closest JDT workspace going from BASE-PATH up"
    (let ((workspace-location (locate-dominating-file base-path "workspace/jdt.ls-java-project")))
      (if (not workspace-location) nil
        (concat workspace-location "workspace/"))))

  (defun yl/switch-jdt-workspace ()
    (interactive)
    (let ((wp-dir (replace-regexp-in-string "~" (getenv "HOME") (yl/determine-jdt-workspace (projectile-project-root)))))
      (setq-default lsp-java-workspace-dir wp-dir)
      (setq-default lsp-java-workspace-cache-dir (concat wp-dir ".cache"))
      (lsp t)))
  (defun yl/mvn-format ()
    (interactive)
    (projectile-run-shell-command-in-root "mvn fmt:format"))

  ;;(add-hook 'projectile-after-switch-project-hook #'yl/switch-jdt-workspace)

  :bind (
         ("C-c j ?" . yl/lsp-ui-doc-toggle)
         ("C-c j f" . yl/mvn-format)
         ("C-c j b" . lsp-java-build-project)
         ("C-c j n" . lsp-java-actionable-notifications)
         ("C-c j c" . lsp-java-update-project-configuration)
         ("C-c j g o" . lsp-java-generate-overrides)
         ("C-c j g g" . lsp-java-generate-getters-and-setters)
         ("C-c j g s" . lsp-java-generate-to-string)
         ("C-c j o" . helm-imenu)
         ("C-c o" . lsp-ui-imenu)
         )
  :hook ((java-mode . lsp))
  )

(use-package terraform-mode
  ;; if using straight
  ;; :straight t
  :ensure t
  :hook ((terraform-mode . lsp))
  :custom (terraform-indent-level 2)
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    ;; (outline-minor-mode 1)
    )
  (setq lsp-terraform-ls-enable-show-reference t)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-semantic-tokens-honor-refresh-requests t)
  (setq lsp-enable-links t)
  (setq lsp-terraform-ls-prefill-required-fields t)
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

;; (use-package terraform-ls
;;   :ensure t
;;   :after lsp
;;   :hook ((terraform-mode . lsp))
;;   :config
;;   )


(use-package which-key
  :ensure t
  :config (which-key-mode)
  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package go-mode
  :ensure t
  :config
  (require 'lsp)
  (defun yl/gofmt()
    "Organize imports and format code"
    (interactive)
    (lsp-organize-imports)
    (gofmt))
  (add-hook 'go-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l f") #'yl/gofmt)
              ))
  :mode (("\\.go$"  . go-mode)))

(use-package golden-ratio
  :ensure t
  :config
  (setq golden-ratio-auto-scale 1)
  (golden-ratio-mode 1))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java :after (lsp-java))

(unless (package-installed-p 'claude-code-ide)
  (package-vc-install
   '(claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")))
(use-package claude-code-ide
  :ensure nil
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup))


(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'light)
  (sml/setup))

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

(use-package keychain-environment
  :ensure t
  :demand
  :config
  (keychain-refresh-environment))

(load-file "~/.emacs.d/utils/buffer.el")
(load-file "~/.emacs.d/utils/uuid.el")

(load-file "~/.emacs.d/treemacs.el")
(load-file "~/.emacs.d/python.el")

;; Copy for WSL2
;; (setq interprogram-cut-function
;;       (lambda (text &optional push)
;;         (with-temp-buffer
;;           (insert text)
;;           (shell-command-on-region (point-min) (point-max) "clip.exe"))))

(defun yl/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(use-package expand-region
  :ensure t)

(use-package bind-key
  :ensure t
  :demand
  :config
  (bind-key (kbd "M-q") 'er/expand-region)
  (bind-key (kbd "s-?") 'xah-copy-file-path)
  (bind-key (kbd "C-c n") 'xah-new-empty-buffer)
  (bind-key (kbd "<M-up>") 'move-text-up)
  (bind-key (kbd "<M-down>") 'move-text-down)
  (bind-key (kbd "C-x C-k") 'yl/kill-this-buffer)
  ;(bind-key* (kbd "C-c C-f") 'with-editor-finish)
  (bind-key (kbd "C-x |") 'toggle-window-split)
  )

(defun my-c-initialization-hook ()
  (define-key java-mode-map "\M-q" 'er/expand-region))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;(global-set-key (kbd "<s-up>") 'previous-error)
;(global-set-key (kbd "<s-down>") 'next-error)
                                        ;(global-set-key (kbd "C-c C-c") "\C-a\C- \C-n\M-w\C-y\C-p") ; duplicate line

                                        ;"sqlformat - --reindent --keywords upper --use_space_around_operators"
(defun my-fsql-on-region (start end)
  "Execute fsql.sh on the region and replace the region with the output."
  (interactive "r")
  (shell-command-on-region start end "/home/yauhen/bin/fsql" t t))
(global-set-key (kbd "C-c |") 'my-fsql-on-region)

(use-package ispell
  :config
  (add-to-list
   'ispell-hunspell-dict-paths-alist
   '("deutsch-hunspell" "/usr/share/hunspell/de_DE.aff"))
  (add-to-list
   'ispell-hunspell-dict-paths-alist
   '("english-hunspell" "/usr/share/hunspell/en_US.aff"))
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


(use-package load-env-vars
  :ensure t
  :demand)

(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(fixed-pitch ((t (:extend t :background "#F5F5DC" :family "DejaVu Sans Mono"))))
 '(highlight-symbol-face ((t (:background "gainsboro"))))
 '(linum ((t (:inherit (shadow default)))))
 '(lsp-ui-sideline-code-action ((t (:foreground "grey"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :foreground "black"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :foreground "black" :weight bold :height 1.2)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(browse-at-remote-preferred-remote-name "raisin")
 '(browse-at-remote-remote-type-regexps
   '(("^github\\.com$" . "github")
     ("^bitbucket\\.org$" . "bitbucket")
     ("^gitlab\\.com$" . "gitlab")
     ("^git\\.savannah\\.gnu\\.org$" . "gnu")
     ("^gist\\.github\\.com$" . "gist")
     ("^git\\.sr\\.ht$" . "sourcehut")
     ("^.*\\.visualstudio\\.com$" . "ado")
     ("^pagure\\.io$" . "pagure")
     ("^.*\\.fedoraproject\\.org$" . "pagure")
     ("^.*\\.googlesource\\.com$" . "gitiles")
     ("git\\.prod\\.infra\\.deposit" . "gitlab")))
 '(claude-code-ide-cli-path "/home/yauhen/.local/bin/claude")
 '(ediff-split-window-function 'split-window-horizontally)
 '(eglot-java-workspace-folder "/home/yauhen/ws/")
 '(flycheck-display-errors-delay 0.3)
 '(forge-alist
   '(("github.com" "api.github.com" "github.com" forge-github-repository)
     ("git.prod.infra.deposit" "git.prod.infra.deposit/api/v4" "git.prod.infra.deposit" forge-gitlab-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org" forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit**-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository)))
 '(git-commit-summary-max-length 128)
 '(helm-move-to-line-cycle-in-source nil)
 '(helm-projectile-truncate-lines t)
 '(helm-use-frame-when-more-than-two-windows nil)
 '(highlight-symbol-idle-delay 0.3)
 '(lsp-enable-file-watchers nil)
 '(lsp-java-boot-enabled nil)
 '(lsp-java-configuration-runtimes
   ["/home/yauhen/.sdkman/candidates/java/11.0.17-amzn" "/home/yauhen/.sdkman/candidates/java/8.0.352-amzn"])
 '(lsp-java-format-comments-enabled nil)
 '(lsp-java-format-enabled nil)
 '(lsp-java-format-on-type-enabled nil)
 '(lsp-java-java-path
   "/home/yauhen/.sdkman/candidates/java/11.0.12-open/bin/java")
 '(lsp-java-jdt-download-url
   "https://download.eclipse.org/jdtls/milestones/1.9.0/jdt-language-server-1.9.0-202203031534.tar.gz")
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx3G" "-Xms100m" "-javaagent:/home/yauhen/.m2/repository/org/projectlombok/lombok/1.18.24/lombok-1.18.24.jar"))
 '(lsp-kotlin-compiler-jvm-target "17")
 '(lsp-session-file "/home/yauhen/.emacs.d/.lsp-session-v2")
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(markdown-xhtml-header-content
   "<style type='text/css'>\12         li p { margin: 0; display: inline; }\12         ul { margin-top: 0; }\12         p { margin-bottom: 0; }\12       </style>")
 '(package-selected-packages
   '(claude-code-ide yafolding typescript-mode flycheck-plantuml plantuml-mode browse-at-remote expand-region helm-ag helm-projectile yasnippet which-key web-mode treemacs-projectile treemacs-magit terraform-mode syntax-subword sqlite3 smart-mode-line org-mind-map lsp-ui lsp-pyright lsp-java load-env-vars keychain-environment helm-lsp helm-file-preview golden-ratio go-mode flycheck-pos-tip crux company color-theme-modern anzu))
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")))
 '(plantuml-default-exec-mode 'executable)
 '(plantuml-executable-path "/home/yauhen/bin/plantuml")
 '(plantuml-jar-path "/home/yauhen/tools/plantuml/plantuml-mit-1.2025.3.jar")
 '(plantuml-java-command
   "/home/yauhen/.sdkman/candidates/java/8.0.302-open/bin/java")
 '(projectile-git-submodule-command "")
 '(projectile-globally-ignored-files '("TAGS" "*.iml"))
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values
   '((eval setenv "CLAUDE_CONFIG_DIR"
           (expand-file-name "~/.claude"))
     (eval setenv "CLAUDE_CONFIG_DIR"
           (expand-file-name "~/.claude-private"))
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))))
 '(typescript-indent-level 2)
 '(warning-suppress-log-types '((comp)))
 '(web-mode-enable-auto-indentation nil)
 '(yas-snippet-dirs '("/home/yauhen/.emacs.d/snippets")))

(provide 'init)
;;; init.el ends here
