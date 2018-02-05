
(require 'package)
(package-initialize)
(setq package-archives '(;("gnu" . "https://elpa.gnu.org/packages/")
                         ;("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ))

;;(package-refresh-contents)

;; Load theme if in GUI
(setq custom-safe-themes t) ;; skip prompt
(when (display-graphic-p) ;; Enable in GUI
  (load-theme 'dracula t)
  )

;; LIFE QUALITY ;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(show-paren-mode 1)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x f") 'find-file) ;; override fill column

;; Copy current line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
		  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s"))
  )
(global-set-key "\C-c\C-k" 'copy-line)

;; TYPING ;;

(electric-pair-mode 1)
(setq electric-pair-pairs '( ;; Additional pairs
			    (?\" . ?\")
			    (?\{ . ?\})
			    ))



;; PACKAGES ;;

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'window-number)
(window-number-mode 1) ;; C-x C-j #
(window-number-define-keys window-number-mode-map "C-x j") ;; avoid mistyping
