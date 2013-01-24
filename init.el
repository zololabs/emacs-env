(setq *EMACS-ENV* "~/.emacs.d")

;; Add Marmalade package archive for Emacs starter kit and other Emacs packages

(require 'package)
(add-to-list 'package-archives
	'("marmalade" . "http://marmalade-repo.org/packages/") )
(package-initialize)


;; Add Clojure and other supporting packages to the Emacs environment
;; Packages are installed if they are not already present
;; The list includes packages for the starter kit, Clojure and markdown files (used by github)

(when (not package-archive-contents)
 	(package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-eshell
                      starter-kit-bindings
                      clojure-mode
                      clojure-test-mode
                      rainbow-delimiters
                      ac-slime
                      markdown-mode
                      popup
                      maxframe
                      auto-complete
                      nrepl
                      ac-nrepl
                      fuzzy))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Useful global settings as Emacs is used predominantely for Clojure development
(global-set-key (kbd "C-c C-j") 'nrepl-jack-in)


;; Colour mach parens and other structure characters to make code easy to follow

(global-rainbow-delimiters-mode)

;;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/twilight")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

;;; Font Size

(set-default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;;; Maximize Frame
(maximize-frame)

(menu-bar-mode)
(load-file (concat *EMACS-ENV* "/init_emacs.el"))
(load-file (concat *EMACS-ENV* "/key_bindings.el"))

;;; ElDoc Mode
(add-hook 'clojure-mode-hook 'eldoc-mode)

;;; Auto Complete
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete/")
(require 'auto-complete-config)
(ac-config-default)

(load-file (concat *EMACS-ENV* "/user_setup.el"))

(load-file (concat *EMACS-ENV* "/patches/clojure-mode-patch.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "0c8ffb272e65e2ffc49b0d5755d7db137931c5e3ed47890d7a6de07717eaa2e8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun my-clojure-mode-hook ()
  (setq indent-tabs-mode nil)
  (push `("\\(demonictest\\|sivatest\\)[ \r\n\t]*\\(\\sw+\\)?"
          (1 font-lock-keyword-face)
          (2 font-lock-function-name-face nil t))
        clojure-font-lock-keywords)
  (dolist (x '((case . 1)
               (condp . 2)
               (are . let)
               (doto . defn)))
    (put (car x)
         'clojure-indent-function
         (if (numberp (cdr x))
             (cdr x)
           (get (cdr x) 'clojure-indent-function))))
  (clojure-test-mode))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)


;;nrepl autocomplete
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
