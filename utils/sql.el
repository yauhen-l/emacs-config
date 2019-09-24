
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (require 'auto-complete-config)
            (setq ac-sources
                  (append '(
                            ac-0-sql-watch-and-register-table-alias
                            ac-1-sql-schemas
                            ac-1-sql-functions
                            ac-1-sql-tables
                            ac-1-sql-columns)
                          ac-sources))
            (setq ac-auto-start t)
            (auto-complete-mode)
            (ac-set-trigger-key "TAB")
            )
          )

(provide 'sql)
;;; sql.el ends here

