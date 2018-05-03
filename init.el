
(require 'package)
(package-initialize)
(add-to-list 'package-archives
			 '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-refresh-contents)

;; Load theme if in GUI
(setq custom-safe-themes t) ;; skip prompt
(when (display-graphic-p) ;; Enable in GUI
(load-theme 'dracula t))

;; LIFE QUALITY ;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)

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

;; TYPING ;;

(electric-indent-mode)
(setq tab-width 2)
(electric-pair-mode 1)
(setq electric-pair-pairs '( ;; Additional pairs
			    (?\" . ?\")
			    (?\{ . ?\})
			    ;;(?\` . ?\`)
			    ))

;; PACKAGES ;;

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'window-number)
(window-number-mode 1) ;; C-x C-j default
(window-number-define-keys window-number-mode-map "<f4>")

(require 'ace-jump-mode)
(define-key global-map (kbd "C-SPC") 'ace-jump-mode)

(global-set-key (kbd "C-x g") 'magit-status)
