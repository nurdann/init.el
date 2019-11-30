;; `C-n` move down a line
;; `C-p` move up a line
;; `C-f` move forward a character
;; `C-b` move backward a character
;; `M-f` move forward a word (alphanumeric sequence)
;; `M-b` move backward a word
;; `C-x C-f` find file and open
;; `C-x d` dired or show directory
;; `C-x b` show buffers which are usually currently opened files
;; `C-x C-c` exit emacs
;; `C-x C-s` save file
;; `C-x k` kill buffer

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; go to line with (add-to-list ...)
;; and press `C-M-x` to evaluate lisp function; or move cursor the end of parenthesis and evaluate last expression with `C-x C-e`
;; then go to command menu `M-x` and type `list-packages` now you will see all available packages for download

;; `S-<arrow-key>`, Shift <arrow-key> to move around windows
;; Some terminology: frame contains windows, so window is the buffer window in Emacs 
(windmove-default-keybindings) 
;; If you don't want to use them, then use default `C-x o` to cycle through windows

;; change default behaviour
(setq ring-bell-function 'ignore ;; disable sound bell on error
      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t 
      window-combination-resize t
)

;; save customization done via Emacs interface to different
;; folder, otherwise Emacs will append to init file 
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))
;; Use `C-h` prefix to get help
;; `C-h f` describe function
;; `C-h v` describe variable
;; `C-h c` describe key sequence
;; `C-h m` describe mode
(menu-bar-mode 1)
(tool-bar-mode -1)
;; you can set fill-column value with `C-x f`
(auto-fill-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)
;; add additional pairs for quotation and curly braces
(setq electric-pair-pairs '((?\" . ?\")
			    (?\{ . ?\})))

;; C-x combo is very common so I remap it to a single key
(global-set-key (kbd "<menu>") ctl-x-map)
;; Then you either set global keybinding or add to a mode map
(define-key ctl-x-map (kbd "f") 'find-file)
;; OR 
(global-set-key (kbd "C-x f") 'find-file)

;; sudo
;; `C-x C-f /sudo::/` press <tab> or a character to get prompt
;; `C-x C-f /ssh:user@host:/`

;; IDO mode
;; enhances behaviour of find-file and switch-buffer
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t)


;; Dired
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)
;; Go to Dired with `C-x d`

;; Find file C

;; CUA mode


;; Evil mode

;; use-package

;; chords
;;(key-chord-define-global k c)

;; jump to minimum strokes
;; avy

