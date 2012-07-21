(defun clojure-find-block-comment-start (limit)
  (let ((pos (re-search-forward "\\((comment\\>\\|#_\\)" limit t)))
    (when pos
      (if (string-equal (match-string 1) "#_")
          (forward-char -2)
        (forward-char -8))
      (point))))

(defun clojure-font-lock-extend-region-comment ()
  "Move fontification boundaries to always contain
  entire (comment ..) sexp. Does not work if you have a
  white-space between ( and comment, but that is omitted to make
  this run faster."
  (let ((changed nil))
    (goto-char font-lock-beg)
    (condition-case nil (beginning-of-defun) (error nil))
    (let ((pos (clojure-find-block-comment-start font-lock-end)))
      (when pos
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (condition-case nil (forward-sexp) (error nil))
        (when (> (point) font-lock-end)
          (setq font-lock-end (point)
                changed t))))
    changed))

(defun clojure-font-lock-mark-comment (limit)
  "Marks all (comment ..) forms with font-lock-comment-face."
  (let (pos)
    (while (and (< (point) limit)
                (setq pos (clojure-find-block-comment-start limit)))
      (when pos
        (condition-case nil
            (add-text-properties (point)
                                 (progn
                                   (forward-sexp)
                                   (point))
                                 '(face font-lock-comment-face multiline t))
          (error (forward-char 8))))))
  nil)

;; this enables the block comment highlighting
(setq clojure-mode-font-lock-comment-sexp t)
