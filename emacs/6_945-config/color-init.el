;;  CHOOSE only ONE of these color themes!

(require 'color-theme)
(color-theme-initialize)
;; required for color-theme-leuven to work
(setq max-lisp-eval-depth 1000)

;; ;; Bright Themes
;; ;; good for printing source code on white paper
;;  (color-theme-sitaramv-nt)

;; ;;; primary colors on white.  Great for Org mode especially
;; (load "color-theme-leuven")
;; (color-theme-leuven)

;; Dark Themes

;;; magical, unobstrusive dark theme
 (load "zenburn")
 (zenburn)

;;; dark theme with lurid colors
;;;  (load "color-theme-colorful-obsolescence")
;;;  (color-theme-colorful-obsolescence)

;;; brillant dark color theme
;;;  (load "color-theme-dark-emacs")
;;;  (color-theme-dark-emacs)

;;; rediculous
;;;  (load "color-theme-manoj")
;;;  (require 'manoj-colors)
;;;  (color-theme-manoj-dark)

