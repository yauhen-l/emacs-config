;;; package --- Yauhen's emacs config
;;; Code:
;;; Commentary:

(use-package lsp-mode
  :init 
  (setq lsp-prefer-flymake nil)
  (add-hook 'java-mode-hook #'lsp)
  :demand t
  :after jmi-init-platform-paths)

(use-package lsp-ui
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t)
  :after lsp-mode)

;; (use-package meghanada
;;   :defer t
;;   :init
;;   ;; (add-hook 'java-mode-hook
;;   ;;           (lambda ()
;;   ;;             (meghanada-mode t)
;;   ;;             (smartparens-mode t)
;;   ;;             (rainbow-delimiters-mode t)
;;   ;;             (highlight-symbol-mode t)
;;   ;;             ;(add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;   ;;             ))

;;   :config
;;   (use-package realgud
;;     :ensure t)
;;   (setq indent-tabs-mode nil)
;;   (setq tab-width 2)
;;   (setq c-basic-offset 2)
;;   (setq meghanada-server-remote-debug t)
;;   (setq meghanada-javac-xlint "-Xlint:all,-processing")
;;   :bind
;;   (:map meghanada-mode-map
;;         ("C-c m t" . meghanada-switch-testcase)
;;         ("C-c m v" . meghanada-local-variable)
;;         ("s-i" . helm-imenu)
;;         ("C-c m r" . meghanada-reference)
;;         ("C-c m f" . java-format-project-files)
;;         ("C-c m i" . meghanada-typeinfo))
;;   :commands
;;   (meghanada-mode))

(defun java-format-project-files()
  (interactive)
  (run-in-project-no-output (string-join (cons "format" (list-project-java-files)) " ")))

(defun java-format-file()
	(interactive)
  (save-buffer)
	(shell-command (concat "format " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm))

(defun list-project-java-files()
  (directory-files-recursively (projectile-project-root) "^[^.].*\\.java\\'"))

(provide 'java)
;;; java.el ends here
