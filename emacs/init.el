;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/")
;; (load-file "~/.emacs.d/6.945-init.el")

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
(dolist (package (list 'auto-complete ))
	(unless (package-installed-p package)
		(package-install package)))

;; (when (memq window-system '(mac ns))
;;       (exec-path-from-shell-initialize))

;; auto update buffers
(global-auto-revert-mode t)


;; ;; Git
;; (autoload 'magit-status "magit" nil t)

;; Auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
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
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Key Bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move to beginning and end of buffer
(global-set-key "\C-xt" 'beginning-of-buffer)
(global-set-key "\C-xe" 'end-of-buffer)
(global-set-key [?\C-\S-b] 'forward-word)
(global-set-key (kbd "C-B") 'backward-word)
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-char)

;; Backward word kill, sometimes it is more efficient 
;; to delete the word and retype it than using backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


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

;; Display file path
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun show-file-name ()
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c b")'show-file-name) ; or any other key you want

;; Indent 
(global-set-key (kbd "C-c i")'indent-region) ; indent

;; Comment
(defalias 'ct 'comment-or-uncomment-region)

;; Alias for query replace regexp
(defalias 'qrr 'query-replace-regexp)

;;;; Info
;; In most cases, this enables to see the invisible text in Info mode
(setq Info-hide-note-references nil)


;; Interactive Do Things
(require 'ido)
(ido-mode t)


;; Font size
(set-face-attribute 'default nil :height 140)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; default emacs configuration directory
(defconst yan:emacs-config-dir "~/.emacs.d/config/" "")

;; utility finction to auto-load my package configurations
(defun yan:load-config-file (filelist)
  (dolist (file filelist)
	  (load (expand-file-name 
		 (concat yan:emacs-config-dir file)))
	  (message "Loaded config file:%s" file)
	  ))

;; load my configuration files
(yan:load-config-file '(
			"programming"
			"mit_6_945"
			))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode matlab-mode scheme-complete scala-mode2 scala-mode powershell magit git geiser fuzzy auto-complete auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


