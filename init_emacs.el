(setq magit-revert-item-confirm t)

(global-hl-line-mode)

;; ido-mode
(require 'ido)
(ido-mode t)

;;; save histories
 (savehist-mode 1)

;;; recent files
(require 'recentf)
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; tabs
(setq-default indent-tabs-mode nil)

;; winner-mode for window configuration management
(winner-mode 1)

;;; highlight-interactive mode
(global-hi-lock-mode 1)

;; save-point in file
(require 'saveplace)
(setq-default save-place t)

;; toggle split windows
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
(define-key ctl-x-4-map "t" 'toggle-window-split)


;; revive
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

;; windmove
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))

;; revert-all-buffers
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (buffer-file-name buffer)
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))

;; column-number-mode
(setq line-number-mode t)

;; y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)


;;
;; find word at point
;;

(defun my-isearch-word-at-point ()
 (interactive)
 (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
 (when (equal this-command 'my-isearch-word-at-point)
   (let ((string (concat "\\<"
                         (buffer-substring-no-properties
                          (progn (skip-syntax-backward "w_") (point))
                          (progn (skip-syntax-forward "w_") (point)))
                         "\\>")))
     (if (and isearch-case-fold-search
              (eq 'not-yanks search-upper-case))
         (setq string (downcase string)))
     (setq isearch-string string
           isearch-message
           (concat isearch-message
                   (mapconcat 'isearch-text-char-description
                              string ""))
           isearch-yank-flag t)
     (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

(global-set-key (kbd "C-x *") 'my-isearch-word-at-point)
