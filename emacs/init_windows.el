;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/")
;; (load-file "~/.emacs.d/6.945-init.el")

;; Cygwin
(if (file-directory-p "c:/cygwin64/bin")
    (add-to-list 'exec-path "c:/cygwin64/bin"))


;; Require common lisp
(require 'cl)

;; Require quack for scheme
(require 'quack)

;; Package archives
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package (list 'auto-complete))
  (unless (package-installed-p package)
    (package-install package)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Git
(autoload 'magit-status "magit" nil t)

;; Auto complete
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)
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

;; ;; Interactive Do Things
;; (require 'ido)
;; (ido-mode t)



;;;; ediff
(setq ediff-split-window-function 'split-window-horizontally)


;;; Scheme
(setq geiser-active-implementations '(racket))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(quack-programs (quote ("scm" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;;; Python
; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)


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

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


;;; Settings for Windows 10
(set-face-attribute 'default nil :height 120)
(set-fontset-font "fontset-default" 'gb18030 '("Microsoft YaHei" . "unicode-bmp"))

(global-auto-revert-mode t)



;; OSX stuff
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)

;; (setq gud-pdb-command-name "python3 -m pdb")

;; (defun dos2unix ()
;;   "Replace DOS eolns CR LF with Unix eolns CR"
;;   (interactive)
;;     (goto-char (point-min))
;;     (while (search-forward "\r" nil t) (replace-match "")))




