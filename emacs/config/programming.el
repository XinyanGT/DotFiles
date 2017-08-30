;; Shortkey to toggle auto fill mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)
;; Don't use TABS for indentations.
(setq-default indent-tabs-mode nil)
;; Set the number to the number of columns to use.
(setq-default fill-column 79)

;;;; Latex Related

;; BibTex
(setq bibtex-align-at-equal-sign t) 
(setq bibtex-autokey-name-case-convert (quote capitalize)) 
(setq bibtex-autokey-name-length -3) 
(setq bibtex-autokey-names 4) 
(setq bibtex-autokey-additional-names "etal") 
(setq bibtex-autokey-names-stretch 0) 
(setq bibtex-autokey-titleword-length 0) 
(setq bibtex-autokey-titlewords 0) 
(setq bibtex-entry-format (quote (opts-or-alts required-fields numerical-fields page-dashes realign delimiters unify-case)))


;; ;; AUC TeX
;; (eval-after-load "tex-mode" '(progn
;;   (add-to-list 'load-path "~/.emacs.d/elisp/auctex/")
;;   (load "auctex.el" nil t t)
;;   (add-to-list 'load-path "~/.emacs.d/elisp/auctex/preview/")
;;   (load "preview-latex.el" nil t t)))
;; (setq Info-default-directory-list
;;   (cons "~/.emacs.d/elisp/auctex/doc/" Info-default-directory-list))
(load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

;; Ref Tex
(setq reftex-plug-into-AUCTeX t)

;; Use png instead of GhostScript
(setq preview-image-type 'png)

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (setq TeX-auto-save t)
    (setq TeX-parse-self t) 
    (setq-default TeX-master nil)
    (reftex-mode t)
    (TeX-fold-mode t)))

;; Generate PDF
  (setq TeX-PDF-mode t)



;;;; VIP
;; Change to vip-mode easily
(global-set-key "\C-xv" 'vip-mode)
(add-hook 'vip-mode-hook
  (lambda ()
    (local-set-key "]" 'vip-scroll)
    (local-set-key "[" 'vip-scroll-back)
    (local-set-key ";" 'vip-beginning-of-line)
    (local-set-key "'" 'vip-goto-eol)))


;; ;;; Scheme
;; ;; Require common lisp
;; (require 'cl)

;; ;; Require quack for scheme
;; (require 'quack)

;; (setq geiser-active-implementations '(racket))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(quack-programs (quote ("scm" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
;; ;; (custom-set-faces
;; ;;  ;; custom-set-faces was added by Custom.
;; ;;  ;; If you edit it by hand, you could mess it up, so be careful.
;; ;;  ;; Your init file should contain only one such instance.
;; ;;  ;; If there is more than one, they won't work right.
;; ;;  )



;;; Python
;; Use IPython
;; pdb setup, note the python version
;; (setq pdb-path '/usr/lib/python3.5/pdb.py
;;       gud-pdb-command-name (symbol-name pdb-path))
;; (defadvice pdb (before gud-query-cmdline activate)
;;   "Provide a better default command line when called interactively."
;;   (interactive
;;    (list (gud-query-cmdline pdb-path
;; 			    (file-name-nondirectory buffer-file-name)))))

(require 'python)
(setq python-shell-interpreter "ipython3")
(setq python-python-command "python3")
;; (setq py-force-py-shell-name-p t)
;; (setq-default py-shell-name "ipython")
;; (setq-default py-shell-name "ipython3")
;; (setq-default py-which-bufname "IPython")

;; ; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
;; ; don't split windows
;; (setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)

(defcustom py-docstring-fill-column 72 [...])
(defcustom py-comment-fill-column 79 [...])

(add-hook 'python-mode-hook 'turn-on-auto-fill)
(setq py-indent-offset 4)
(setq indent-tabs-mode nil)
;; (setq python-shell-interpreter "ipython3"
;;             python-shell-interpreter-args "-i")
; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))


; restart pdb session by R
(defun pdb-restart ()
  (interactive)
  (comint-insert-send "restart")
  (sleep-for .5)
  (when
      (or
       (last-lines-match "raise Restart.*
Restart")
       (last-lines-match "restart")
       (not (get-buffer-process (current-buffer)))
       )

    (let (
      (kill-buffer-query-functions nil );disable confirming for process kill
      (pdbcmd (car-safe (symbol-value (gud-symbol 'history nil 'pdb))))
      (default-directory default-directory)
      )
      (kill-this-buffer)
      (cd default-directory)
      (pdb pdbcmd))
    )
  (comint-insert-send "n")
)
(defun comint-insert-send (input)
  (insert input)
  (comint-send-input)
)

(defun last-lines-match (regexp &optional n)
  (setq n (or n 3))
  (re-search-backward regexp (line-beginning-position (- 0 n)) t))
(require 'gud)   
(define-key gud-mode-map "R" 'pdb-restart)


