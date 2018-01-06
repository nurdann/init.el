
(require 'package)
(package-initialize)
(setq package-archives '(;("gnu" . "https://elpa.gnu.org/packages/")
                         ;("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ))

;;(package-refresh-contents)

;; Load theme if in GUI
(setq custom-safe-themes t)
(when (display-graphic-p)
  (load-theme 'dracula t)
  )

;; LIFE QUALITY ;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(show-paren-mode 1)


;; TYPING ;;

(electric-pair-mode 1)
(setq electric-pair-pairs '(
			    (?\" . ?\")
			    (?\{ . ?\})
			    ))

;; PACKAGES ;;

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'window-number)
(window-number-mode 1)
