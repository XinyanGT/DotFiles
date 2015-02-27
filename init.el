;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require common lisp
(require 'cl)

;; Package archives
(require 'package)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Git
(autoload 'magit-status "magit" nil t)

;; Auto complete
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'nxml-mode)
(add-to-list 'ac-modes 'rst-mode)

;; Print
;; Which does NOT work!!
;;(setenv "GS_LIB" "D:/gs9.14/lib/")
;;(setq ps-lpr-command "D:/gs9.14/bin/gswin32c.exe")
;;(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
;;(setq ps-printer-name t)

;; Display line number
(global-linum-mode 1)

;; Turn off UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

 ;; Auto indention
(add-hook 'c-mode-common-hook '(lambda ()
  (local-set-key(kbd "RET") 'newline-and-indent)))

(add-hook 'nxml-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'python-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; Backup files
(setq vc-make-backup-files t)
(setq version-control t     ;; Use version numbers for backups.
   kept-new-versions 6      ;; Number of newest versions to keep.
   kept-old-versions 2      ;; Number of oldest versions to keep.
   delete-old-versions t    ;; Don't ask to delete excess backup versions.
   backup-by-copying t)     ;; Copy all files, don't rename them.
(setq backup-directory-alist `((".*" . ,"~/.emacs.d/backup/per-save")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup/per-save t")))
(defun force-backup-of-buffer ()
    ;; Make a special "per session" backup at the first save of each
    ;; emacs session.
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
            (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save.  The first save results in
    ;; both a per-session and a per-save backup, to keep the numbering
    ;; of per-save backups consistent.
   (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup/per-session t")))


;; Auto save desktop
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move to beginning and end of buffer
(global-set-key "\C-xt" 'beginning-of-buffer)
(global-set-key "\C-xe" 'end-of-buffer)

;; Backward word kill, sometimes it is more efficient 
;; to delete the word and retype it than using backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Change to vip-mode easily
(global-set-key "\C-xv" 'vip-mode)

;; Invoke M-x without Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Paren
(global-set-key "\C-xp" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	                  (t (self-insert-command (or arg 1)))))
;; Compile
(global-set-key "\C-xc" 'compile)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode) ; sorted by major-mode

;; Move cursor between frames
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; Change between window layouts
(winner-mode 1)
(global-set-key (kbd "C-c [")  'winner-undo)
(global-set-key (kbd "C-c ]") 'winner-redo)

;; Comment
(defalias 'ct 'comment-or-uncomment-region)

;; Alias for query replace regexp
(defalias 'qrr 'query-replace-regexp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode Specifics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; AUC TeX
(eval-after-load "tex-mode" '(progn
  (add-to-list 'load-path "~/.emacs.d/elisp/auctex/")
  (load "auctex.el" nil t t)
  (add-to-list 'load-path "~/.emacs.d/elisp/auctex/preview/")
  (load "preview-latex.el" nil t t)))
(setq Info-default-directory-list
  (cons "~/.emacs.d/elisp/auctex/doc/" Info-default-directory-list))

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

;;;; Info
;; In most cases, this enables to see the invisible text in Info mode
(setq Info-hide-note-references nil)


;;;; VIP
(add-hook 'vip-mode-hook
  (lambda ()
    (local-set-key "]" 'vip-scroll)
    (local-set-key "[" 'vip-scroll-back)
    (local-set-key ";" 'vip-beginning-of-line)
    (local-set-key "'" 'vip-goto-eol)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; from python.el
(require 'python)
(setq
 python-shell-interpreter "ipython"
; python-shell-interpreter-args "--gui=wx --matplotlib=wx --colors=Linux"
 python-shell-interpreter-args "--gui=wx --colors=Linux"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

