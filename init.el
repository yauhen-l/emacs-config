;;; package --- Yauhen's emacs config
;;; Code:
;;; Commentary:

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(require 'find-lisp)
(require 'imenus)

(use-package ensime
  :ensure t
  :pin melpa-stable)

(defun run-in-project(command)
	(interactive "s")
	(projectile-with-default-dir (projectile-project-root)
    (let ((buffer-name (concat "*run-in-project: " command  "*")))
      (with-output-to-temp-buffer buffer-name
        (shell-command command buffer-name)))))

(defun run-in-project-no-output(command)
	(interactive "s")
	(projectile-with-default-dir (projectile-project-root)
    (shell-command command)))

(add-hook 'js-mode-hook (lambda ()
													(local-set-key (kbd "C-c r") 'nodejs-run-file)
                          (flymake-json-maybe-load)
                          ))
(add-hook 'json-mode 'flymake-json-load)
(defun nodejs-run-file()
	(interactive)
	(shell-command (concat "node " (buffer-file-name))))

(use-package highlight-symbol
  :bind
  ("s-." . highlight-symbol-next)
  ("s-," . highlight-symbol-prev))
(highlight-symbol-mode t)

(setq tags-revert-without-query 1)

(global-set-key (kbd "s-`") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-a") 'helm-projectile-ag)
(global-set-key (kbd "s-p") 'go-local-playground)
(global-set-key (kbd "s-w") 'mark-a-word-or-thing)
;; Show flycheck summary
(global-set-key (kbd "s-l") (kbd "C-c ! l"))
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
;; Jump between errors
(global-set-key (kbd "<s-down>") (kbd "C-c ! n"))
(global-set-key (kbd "<s-up>") (kbd "C-c ! p"))
(global-set-key (kbd "s-?") 'show-file-name)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-f j") 'json-reformat-region)
(global-set-key (kbd "s-g") 'magit-status)
(global-set-key (kbd "s-h") 'helm-resume)
(global-set-key (kbd "s-<f5>") 'revert-buffer)
(global-set-key (kbd "s-n") 'xah-new-empty-buffer)
(global-set-key (kbd "s-<f2>") 'rename-file-and-buffer)
(global-set-key (kbd "s-I") 'helm-etags-select)
(global-set-key (kbd "s-i") 'imenu)
(global-set-key (kbd "s-u") 'lower-and-concat)
(global-set-key (kbd "s-q") 'er/expand-region)
(global-set-key (kbd "M-s-l") 'magit-log-buffer-file)


(global-set-key (kbd "C-c b") 'magit-blame)
(global-set-key (kbd "C-c F") '(lambda()
                                 (interactive)
                                 (helm-do-ag (concat (getenv "JUNO") "/"))))
(global-set-key (kbd "C-c f") 'helm-do-ag)
(global-set-key (kbd "C-c s") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c k") 'my/quit-bottom-side-windows)
(global-set-key (kbd "C-c h g") 'helm-go-package)
(global-set-key (kbd "C-c D")  'delete-file-and-buffer)
;; Duplicate line
(global-set-key (kbd "C-c C-c") "\C-a\C- \C-n\M-w\C-y")

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(global-set-key (kbd "C-.") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-.") 'ac-complete-with-helm)

(set-face-attribute 'eldoc-highlight-function-argument nil :underline nil :foreground "red" :weight 'bold)
(set-face-attribute 'region nil :background "#BBB")

(use-package flycheck
  :config
  (flycheck-pos-tip-mode)
  (set-face-attribute 'flycheck-error nil :underline (list :color "red" :style 'wave) :foreground nil :background nil)
  (set-face-attribute 'flycheck-warning nil :underline (list :color "yellow" :style 'wave) :background nil :foreground nil))

(linum-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
;;(scroll-bar-mode -1)

(setq tab-width 2)
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

(toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;(desktop-save-mode 1)

(drag-stuff-global-mode)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

(delete-selection-mode 1)

(require 'helm-projectile)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;;(setq projectile-switch-project-action 'projectile-dired)

(setq make-backup-files nil)
(setq auto-save-default nil)

(global-set-key (kbd "C-x C-k") 'close-all-buffers)

(require 'golden-ratio)
(golden-ratio-mode 1)

(require 'yasnippet)
;(setq-default yas-snippet-dirs '("~/.emacs.d/snippets")
(add-to-list 'yas-snippet-dirs "~/Misc/emacs/yasnippet-custom")
(yas-global-mode 1)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml.tmpl$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml.tmpl$" . yaml-mode))

(eval-after-load "git-link"
  '(progn
    (add-to-list 'git-link-remote-alist
      '("git.junolab.net" git-link-bitbucket))
    (add-to-list 'git-link-commit-remote-alist
		 '("git.junolab.net" git-link-commit-bitbucket))))

(load-file "~/.emacs.d/utils/buffer.el")
(load-file "~/.emacs.d/utils/string.el")
(load-file "~/.emacs.d/utils/golang.el")
(load-file "~/.emacs.d/utils/gotests.el")

(buffer-on-bottom-side "*go-guru-output*" "*Ediff Control Panel*" "^\\*[^magit].+\\*$")

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq tags-add-tables nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-at-remote-remote-type-domains
   (quote
    (("bitbucket.org" . "bitbucket")
     ("github.com" . "github")
     ("gitlab.com" . "gitlab")
     ("git.junolab.net" . "github"))))
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ensime-completion-style (quote company))
 '(ensime-graphical-tooltips t)
 '(ensime-use-helm t)
 '(flycheck-display-errors-delay 1.0)
 '(flycheck-go-build-executable "gb")
 '(git-commit-summary-max-length 256)
 '(go-impl-aliases-alist
   (quote
    (("sc" . "sql.Scanner")
     ("val" . "driver.Valuer")
     ("valid" . "types.Validatable")
     ("m" . "json.Marshaler")
     ("um" . "json.Unmarshaler"))))
 '(go-test-case-command "gb test -v -test.run")
 '(golden-ratio-exclude-buffer-names (quote ("*compilation*")))
 '(golden-ratio-exclude-modes nil)
 '(helm-ag-base-command "ag -f --nocolor --nogroup")
 '(highlight-symbol-idle-delay 0.3)
 '(inhibit-startup-screen t)
 '(interprogram-paste-function (quote x-cut-buffer-or-selection-value) t)
 '(json-reformat:indent-width 4)
 '(json-reformat:pretty-string\? t)
 '(package-selected-packages
   (quote
    (ac-helm ag anzu avy benchmark-init browse-at-remote color-theme-modern drag-stuff elpy ensime expand-region flycheck flycheck-pos-tip flycheck-scala-sbt flycheck-tip flymake-json format-sql git-link git-timemachine go-autocomplete go-direx go-dlv go-eldoc go-guru go-impl go-projectile go-rename go-stacktracer golden-ratio gotest helm-ag helm-ag-r helm-go-package helm-projectile highlight-symbol howdoi imenu-anywhere imenus json-mode lua-mode magit move-text nodejs-repl projectile-ripgrep swiper thing-cmds use-package web-beautify web-mode yafolding yaml-mode yasnippet)))
 '(scroll-conservatively 1000)
 '(scroll-margin 10)
 '(select-enable-clipboard t)
 '(semantic-mode t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(sql-indent-maybe-tab t)
 '(sql-indent-offset 2)
 '(sql-mysql-login-params (quote (user password server database port)))
 '(sql-port 3306)
 '(sql-server "localhost")
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-width 20 t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 110 :width normal))))
 '(highlight-symbol-face ((t (:background "gainsboro")))))

(provide 'init)
;;; init.el ends here
