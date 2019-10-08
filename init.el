;; C-h c <command> to get output of command sequence

(require 'package)

(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)

(eval-when-compile (require 'use-package))

(use-package creamsody-theme :ensure :defer)
(use-package parchment-theme :ensure :defer)

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . parchment)
			   ("19:00" . creamsody)))
  (circadian-setup))


;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
;(global-linum-mode t)
;(setq cursor-type '(hbar . 4))
(auto-fill-mode -1)
;(scroll-bar-mode -1)
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;(if (display-graphic-p)
;    (global-unset-key (kbd "C-z")))


(define-key input-decode-map "\C-i" [C-i])
(define-key input-decode-map "\C-m" [C-m])
(define-key input-decode-map "\C-j" [C-j])

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'display-buffer-alist
	     '("^\\*.*\\*$" . (display-buffer-same-window)))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  ;;:bind ("C-x C-p" . keyfreq-show)
  )

;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-'") (lambda () (progn (
					       ()))))
(global-set-key (kbd "C-c C-k") 'copy-current-line)

;(global-set-key (kbd "") 'revert-buffer)

(show-paren-mode 1)

(use-package winner
  :ensure t
  :config (winner-mode 1)
  :bind (("C-c <left>" . undo)
	 ("C-c <right>" . redo)))

(use-package electric
  :ensure t
  :config
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\" . ?\")
			      (?\{ . ?\}))))

(use-package auto-complete
  :init (auto-complete-mode t)
  :config
  (ac-set-trigger-key "<tab>")
  (ac-config-default)
  (setq ac-delay 0.02
	))

(use-package keyfreq
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :bind (("C-?" . undo-tree-redo)
	 ("C-/" . undo-tree-undo)))

(global-set-key (kbd "C-x s") 'save-buffer) ;; same as C-x C-s

;(global-set-key (kbd "s-x") 'delete-other-windows)
;(global-set-key (kbd "s-c") 'split-window-below)
;(global-set-key (kbd "s-v") 'split-window-right)
;(global-set-key (kbd "s-b") 'delete-window)

(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

;(use-package evil
;  :init
;  (evil-mode t))


(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(use-package ace-jump-mode
  :ensure t	     
  :bind (("C-:" . ace-jump-char-mode)
	 ("C-;" . ace-jump-word-mode)))

(use-package ace-window
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?f ?g)) ;; limit characters
  :bind (("C-x o" . ace-window)
	 ("M-o" . ace-window)))

;; FILE NAVIGATION
;(global-unset-key (kbd "C-x f"))
;(define-key key-translation-map (kbd "C-x f") (kbd "C-x C-f"))

;(use-package helm
;  :init
;  (helm-mode t)
;  (helm-autoresize-mode t) ;; grow buffer as needed
;  (setq helm-split-window-in-side-p t ;; split based on current buffer
;	helm-move-to-line-cycle-in-source t ;; cycle options when reaching end/start of buffer
;	helm-autoresize-max-height 50
;	;helm-autoresize-min-height 25
;	)
;  :bind (("M-x" . helm-M-x)
;	 ("C-x f" . helm-find-files)
;	 ("C-x b" . helm-buffers-list)
;	 ("C-x C-f" . helm-recentf)
;	 :map helm-find-files-map
;	 ("C-<del>" . helm-find-files-up-one-level)))

(use-package ido
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (ido-everywhere t)
  :bind (("C-x f" . ido-find-file)
	 ("C-x C-f" . ido-find-file)))

;;;;;;;;;;;;;;;;;;;;
;; Modes
;;;;;;;;;;;;;;;;;;;;

;(autoload 'wikipedia-mode "./elpa/wikipedia-mode/wikipedia-mode.el" "Major mode for wiki" t)

;; Matlab
;(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;(setq matlab-indent-function t)
;(setq matlab-shell-command "matlab")
;
;(add-to-list 'load-path "~/.emacs.d/packages/ematlab")
;(load-library "matlab")
;
;(define-key matlab-mode-map (kbd "C-c l") 'matlab-shell-run-cell)
;(define-key matlab-mode-map (kbd "C-c C-l") 'matlab-shell-run-region)
;(define-key matlab-mode-map (kbd "C-S-l") 'matlab-shell-save-and-go)


;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;

(defun copy-current-line ()
  (interactive)
  (kill-ring-save (line-beginning-position)
				  (line-end-position))
  (message "Copied current line"))


;;;;;;;;;;;;;;;;;;;;
;; CUSTOM MODES
;;;;;;;;;;;;;;;;;;;;

(define-minor-mode fox-mode
  "Toggle fox mode"
  :init-value nil
  :lighter " Fox" ;; indicator for mode line
  :keymap
  '(("f" . forward-char))
  :group 'fox)
(global-unset-key (kbd "<insertchar>"))
(global-unset-key (kbd "<insert>"))
(global-set-key (kbd "<insert>") 'fox-mode)

