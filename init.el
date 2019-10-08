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
(load custom-file)

(global-unset-key (kbd "C-z"))
		  
;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

;(switch-to-buffer (shell-get-buffer-create))
;(put 'mini)

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

(global-set-key (kbd "C-c C-k") 'copy-current-line)

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
  :init (global-undo-tree-mode)
  :bind (("C-S-z" . undo-tree-redo)
	 ("C-z" . undo-tree-undo)))

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

(define-key input-decode-map "\C-i" [C-i])
(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

(use-package ace-jump-mode
  :ensure t	     
  :bind (("M-S-j" . ace-jump-char-mode)
	 ("M-j" . ace-jump-word-mode)))

(use-package ace-window
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?w ?e)) ;; limit characters
  :bind (("M-a" . ace-window)))

;(use-package helm
;  :init
;  (helm-mode t)
;  (helm-autoresize-mode t) ;; grow buffer as needed
;  (setq helm-split-window-in-side-p t ;; split based on current buffer
;	helm-move-to-line-cycle-in-source t ;; cycle options when reaching end/start of buffer
;	helm-autoresize-max-height 50
;					;helm-autoresize-min-height 25
;	)
;  :bind (("M-x" . helm-M-x)
;	 ("C-x f" . helm-find-files)
;	 ("C-x b" . helm-buffers-list)
;	 ("C-x C-f" . helm-recentf)
;	 :map helm-find-files-map
;	 ("DEL" . helm-find-files-up-one-level)))
;

(use-package ido
  :config (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  :bind
  (("M-," . ido-find-file)
   ("M-." . ido-switch-buffer)))

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
;; CUSTOM MODES
;;;;;;;;;;;;;;;;;;;;

;; navigation mode

(define-minor-mode fox-mode
  "Toggle Fox Mode"
  :init-value nil
  :lighter " Fox Mode"
  :keymap
  '(("f" . "<LEFT>"))
  :group 'fox)

;(rebind-key ("C-'" 'fox-mode))
(global-set-key (kbd "C-'") 'fox-mode)
(define-key fox-mode-map (kbd "h") (kbd "<left>"))
(add-key-to-map fox-mode-map "j" (kbd "<down>"))

;; <menu> mode

(progn
  (define-prefix-command 'menu-key-map)
  (define-key menu-key-map (kbd "1") 'delete-other-windows))

(global-set-key (kbd "<menu>") 'menu-key-map)


;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;

(defun copy-current-line ()
  "Copy current line the cursor in"
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))
  (message "Copied current line"))

(defun rebind-key (key out)
  "Unbind then bind key"
  (interactive)
  (global-unset-key (kbd key))
  (global-set-key (kbd key) out))

(defun search-next-char (c)
  "Search next character match"
  (interactive)
  (if (char-equal (char-after 1) c)
      (message "found")
    (message "not")))

(defun add-key-to-map (map key out)
  "Unbind and bind the key in the key map"
  (interactive)
  (define-key map (kbd key) nil)
  (define-key map (kbd key) out))

(defun add-keys-to-map (map list)
  "Bind keys in the list to the map"
  (interactive)
  (if (not (null list))
      (cons (car list) (cdr (add-keys-to-map map '(cdr list))))
    (4)))

;; TODO 
(add-keys-to-map fox-mode-map '((a f) (b g)))

(cons 'a '(f g d))

(let list
      '(("cf" . 23)
	("ff" . 2)))
(assoc "cf" list)

