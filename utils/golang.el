;;; package --- Yauhen's emacs config
;;; Code:
;;; Commentary:

(defun find-not-suffix(l suffix)
  (if (string-suffix-p (car l) suffix)
      (find-not-suffix (cdr l) suffix)
    (car l)))

(defvar init-GOPATH (find-not-suffix (split-string (getenv "GOPATH") ":") "vendor"))

(setq gofmt-command "goimports")
;(projectile-register-project-type 'go #'projectile-go :compile "go build" :test "go test -v")
;(require 'go-autocomplete)
(require 'go-eldoc)
(require 'go-complete)
(add-hook 'completion-at-point-functions 'go-complete-at-point)
;(require 'go-mode-autoloads)
(require 'flycheck)
;(flycheck-define-checker go-build
;  "A Go syntax and type checker using the `go build' command."
;  :command ("go" "build" "./...")
;  :error-patterns ((error line-start (file-name) ":" line ":" (message) line-end))
;  :modes go-mode)

(flycheck-define-checker go-test
  "A Go syntax and type checker using the `go test' command."
  :command ("go" "test" "./...")
  :error-patterns ((error line-start (file-name) ":" line ":" (message) line-end))
  :predicate (lambda ()
               (and (flycheck-buffer-saved-p)
                    (string-suffix-p "_test.go" (buffer-file-name))))
  :modes go-mode)

;(add-to-list 'flycheck-checkers 'go-build)
(add-to-list 'flycheck-checkers 'go-test)
(setq-default flycheck-disabled-checkers '(go-vet go-golint))

(load-file "~/.emacs.d/utils/gotn.el")
(load-file "~/.emacs.d/utils/gotests.el")
(require 'gotn)

(require 'go-impl)

(add-hook 'go-mode-hook (lambda ()
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (add-hook 'after-save-hook 'go-generate-etags)
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                                                    (local-set-key (kbd "C-c o") 'godoc-at-point)
                                                    (local-set-key (kbd "C-c r") 'go-run-file)
                                                    (local-set-key (kbd "C-c t") 'go-test-package)
                                                    (local-set-key (kbd "C-c C-t") 'gotn-run-test)
                          (local-set-key (kbd "<f5>") 'go-guru-describe)
                          (local-set-key (kbd "<f6>") 'go-guru-referrers)
                          (local-set-key (kbd "s-b") 'gb-build-project)
                          (go-guru-hl-identifier-mode)
                          (go-eldoc-setup)
                                                    ))

(defun go-generate-etags()
  (interactive)
  (run-in-project-no-output (string-join (cons "tago" (list-project-go-files)) " ")))

(defun go-package-name(file)
    (replace-regexp-in-string (concat init-GOPATH "/src/") "" file))

(defun go-test-package()
    (interactive)
    (shell-command "go test . -v"))

(defun gb-build-project()
    (interactive)
    (projectile-compile-project "go build ./..."))

(defun go-run-file()
    (interactive)
    (shell-command (concat "go run ." ;(buffer-file-name)
                           )))

(defun go-local-playground()
    (interactive)
    (let ((tmp-go-file (concat init-GOPATH
                             "/src/playground/"
                                                         (replace-regexp-in-string "\\." "playground\/" (number-to-string (float-time)))
                                                         ".go")))
        (let ((dir (file-name-directory tmp-go-file)))
            (make-directory dir)
            (copy-file "~/.emacs.d/gotmpl.go" tmp-go-file)
            (find-file tmp-go-file))))

(defun go-juno-pkg-alias(url)
  (let ((import (split-string url "/"))
        (skipProj '("ms" "ms_core")))
    (let ((host (pop import))
          (proj (pop import))
          (pkg (car (last import))))
      (add-to-list 'skipProj (projectile-project-name))
      (when (and pkg
             (string= host "junolab.net")
             (not (member proj skipProj)))
        (concat (string-remove-prefix "lib_" (string-remove-prefix "ms_" proj)) "_" pkg)))))

;(go-juno-pkg-alias "junolab.net/lib_gis/api")

(defun list-project-go-files()
  (directory-files-recursively (projectile-project-root) "^[^.].*\\.go\\'"))
 
(defun go-import-add1 (arg import)
  "Add a new IMPORT to the list of imports.

When called with a prefix ARG asks for an alternative name to
import the package as.

If no list exists yet, one will be created if possible.

If an identical import has been commented, it will be
uncommented, otherwise a new import will be added."

  ;; - If there's a matching `// import "foo"`, uncomment it
  ;; - If we're in an import() block and there's a matching `"foo"`, uncomment it
  ;; - Otherwise add a new import, with the appropriate syntax
  (interactive
   (list
    current-prefix-arg
    (replace-regexp-in-string "^[\"']\\|[\"']$" "" (completing-read "Package: " (go--old-completion-list-style (go-packages))))))
  (save-excursion
    (let ((as (go-juno-pkg-alias import)) line import-start)
      (if arg
          (setq as (read-from-minibuffer "Import as: ")))
      (if as
          (setq line (format "%s \"%s\"" as import))
        (setq line (format "\"%s\"" import)))

      (goto-char (point-min))
      (if (re-search-forward (concat "^[[:space:]]*//[[:space:]]*import " line "$") nil t)
          (uncomment-region (line-beginning-position) (line-end-position))
        (cl-case (go-goto-imports)
          ('fail (message "Could not find a place to add import."))
          ('block-empty
           (insert "\n\t" line "\n"))
          ('block
              (save-excursion
                (re-search-backward "^import (")
                (setq import-start (point)))
            (if (re-search-backward (concat "^[[:space:]]*//[[:space:]]*" line "$")  import-start t)
                (uncomment-region (line-beginning-position) (line-end-position))
              (insert "\n\t" line)))
          ('single (insert "import " line "\n"))
          ('none (insert "\nimport (\n\t" line "\n)\n")))))))

(provide 'golang)
;;; golang.el ends here
