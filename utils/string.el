;;; package --- Yauhen's string utils
;;; Code:
;;; Commentary:

(defun lower-and-concat (b e)
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (goto-char (point-min))
    (downcase-region b e)
    (while (re-search-forward "[ \t]+" nil t)
      (replace-match "_"))))

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


(provide 'string-utils)
;;; string-utils.el ends here
