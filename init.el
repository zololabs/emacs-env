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

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell starter-kit-bindings
	clojure-mode clojure-test-mode
        rainbow-delimiters
        ac-slime
	markdown-mode ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Useful global settings as Emacs is used predominantely for Clojure development

;; Launch the Clojure repl via Leiningen - M-x clojure-jack-in 
;; Global shortcut definition to fire up clojure repl and connect to it

(global-set-key (kbd "C-c C-j") 'clojure-jack-in)


;; Colour mach parens and other structure characters to make code easy to follow

(global-rainbow-delimiters-mode)

;;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'wombat t)

;;; Font Size

(set-default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;;; Enabling Clojure Test Mode

