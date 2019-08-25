;; C-h c <command> to get output of command sequence

(require 'package)

(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(package-initialize)
;(package-refresh-contents)

(eval-when-compile (require 'use-package))

;(setq custom-safe-themes t) ;; skip prompt
;(when (display-graphic-p)
;  (load-theme 'dracula t))

(use-package circadian
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

;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(show-paren-mode 1)
;(global-set-key (kbd "M-[") 'insert-pair)
;(global-set-key (kbd "M-{") 'insert-paren)

;; Allow undo/redo
;; Default C-c left/right arrow
(use-package winner
  :config (winner-mode 1)
  :bind (("C-c <left>" . undo)
	 ("C-c <right>" . redo)))

;(electric-indent-mode)
(use-package electric
  :config
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\" . ?\")
			      (?\{ . ?\}))))

;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

(windmove-default-keybindings) ;; Shift <arrow-key> to move windows

(require 'window-numbering)
(window-numbering-mode 1)

(use-package helm
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
