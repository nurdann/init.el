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

;; CUA mode
;; default C-z undo, C-x cut, C-c copy, C-v paste
;; C-<enter> for rectangle edit; press <enter> to cycle through corners of selection
;; C-<number> use universal argument to save region to register <number>
(setq cua-delete-selection nil) 	
(cua-mode t)

;; `S-<arrow-key>`, Shift <arrow-key> to move around windows
;; Some terminology: frame contains windows, so window is the buffer window in Emacs 
(windmove-default-keybindings)

;; change default behaviour
(setq ring-bell-function 'ignore ;; disable sound bell on error
      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t 
      window-combination-resize t)

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

;; Find file

;; sudo
;; `C-x C-f /sudo::/` press <tab> or a character to get prompt
;; `C-x C-f /ssh:user@host:/`;; 

;; IDO mode
;; enhances behaviour of find-file and switch-buffer
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t)


;; Dired `C-x d'
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)

;; Terminal options
;; `eshell' written in elisp
;; `shell' uses dumb shell
;; `term' mimics closest usual terminal emulator; note that all `C-x' become `C-c' while `term' buffer is focused, so if you map it to something like `<menu>' you don't have to worry about it
;; Launch with `M-x' and type one of above names

;; Evil mode

;; use-package

;; chords
;;(key-chord-define-global k c)

;; jump to minimum strokes
;; avy

