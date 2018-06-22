
(require 'package)

(add-to-list 'package-archives
			 '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
;;(package-refresh-contents)

;; Load theme if in GUI
(setq custom-safe-themes t) ;; skip prompt
(when (display-graphic-p) ;; Enable in GUI
(load-theme 'dracula t))

;; LIFE QUALITY ;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)

(setq cursor-type '(hbar . 4))

(global-set-key (kbd "C-x f") 'find-file) ;; override fill column

(global-set-key (kbd "C-c C-S-k") (lambda () (interactive) (copy-lines 1)))
(global-set-key (kbd "C-c C-k") 'copy-current-line)

;; Parentheses
(show-paren-mode 1) ;; M-(
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-paren)

;; Copy current line with new line
(defun copy-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
				  (line-beginning-position (+ 1 arg))
				  )
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-current-line ()
  (interactive)
  (kill-ring-save (line-beginning-position)
				  (line-end-position))
  (message "Copied current line"))

(defun insert-paren ()
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?{ ?})
     (backward-paragraph)))

;; Allow undo/redo
;; Default C-c left/right arrow
(winner-mode 1)

;; TYPING ;;

;; auto insert pairs of (), []
(electric-indent-mode)

(electric-pair-mode 1)

;; Additional pairs
(setq electric-pair-pairs '(
			    (?\" . ?\")
			    (?\{ . ?\})
			    ;;(?\` . ?\`)
					))

;; auto complete
(ac-config-default)
(setq ac-ignore-case nil)

;; Navigating ;;

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;(require 'window-number)
;(window-number-mode 1)
;; C-x C-j default
;; remap it to another key
;(window-number-define-keys window-number-mode-map "<f4>")

(require 'ace-jump-mode)
(define-key global-map (kbd "C-SPC") 'ace-jump-mode)

;; To show files on buffer
(require 'ido)
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)
;; Move between windows
;; Shift <arrow-key>
(windmove-default-keybindings)


(require 'window-numbering)
(window-numbering-mode 1)

;; Version Control ;;
(global-set-key (kbd "C-x g") 'magit-status)

;; Matlab
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

(add-to-list 'load-path "~/.emacs.d/packages/ematlab")
(load-library "matlab")

(define-key matlab-mode-map (kbd "C-c l") 'matlab-shell-run-cell)
(define-key matlab-mode-map (kbd "C-c C-l") 'matlab-shell-run-region)
(define-key matlab-mode-map (kbd "C-S-l") 'matlab-shell-save-and-go)
