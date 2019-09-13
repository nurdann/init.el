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
  (setq circadian-themes '(("8:00" . creamsody)
			   ("19:00" . parchment)))
  (circadian-setup))


;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)
;(setq cursor-type '(hbar . 4))

(global-unset-key (kbd "C-z"))
		  
(global-set-key (kbd "C-c C-k") 'copy-current-line)

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

(show-paren-mode 1)

(use-package winner
  :ensure t
  :config (winner-mode 1)
  :bind (("C-c <left>" . undo)
	 ("C-c <right>" . redo)))

;(electric-indent-mode)
(use-package electric
  :ensure t
  :config
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\" . ?\")
			      (?\{ . ?\}))))



;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :ensure t	     
  :bind ("C-j" . ace-jump-mode))

(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

;;(require 'window-numbering)
;;(window-numbering-mode 1)
;(use-package window-numbering
;  :ensure t
;  :config (window-numbering-mode 1))

(use-package ace-window
  :ensure t
  :init
  (ace-window 1)
  (setq aw-keys '(?a ?s ?d ?f ?g))
  :bind ("C-;" . 'ace-window))

(use-package helm
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (setq autoresize-max-height 40
	helm-split-window-in-side-p t)
  :ensure t	     
  :bind (("M-x" . helm-M-x)
	 ("C-x f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-recentf)
	 :map helm-find-files-map
	 ("DEL" . helm-find-files-up-one-level)))


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




;; Emacs generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-tramp haskell-mode dired-toggle-sudo auto-complete flycheck markdown-mode helm ace-window sudo-edit window-numbering use-package parchment-theme keyfreq creamsody-theme circadian ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
