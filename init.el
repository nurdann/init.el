;; C-h c <command> to get output of command sequence

;; TODO
;; Go outside brackets C-M-u C-M-n
;; map:
;; M-x flush-lines
;; M-x load-file
;; M-x revert-buffer

(require 'package)

(setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stable" . "https://stable.melpa.org/packages/")
	    ("melpa" . "https://melpa.org/packages/"))
	  package-archive-priorities
	  '(("melpa-stable" . 10)
	    ("gnu" . 5)
	    ("melpa" . 0)))

(package-initialize)
;;(setq package-check-signature  nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package))
  (require 'bind-key)) ;; required for :bind

;; Theme
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

(setq ring-bell-function 'ignore ;; disable sound bell on error
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t 
      ;;indent-tabs-mode nil
      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      tab-width 4
      set-mark-command-repeat-pop t ;; After C-u C-SPC, C-SPC cycles through the mark ring
      mark-ring-max 16 
      window-combination-resize t
      shift-select-mode t)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; GUI
(menu-bar-mode 1)
(tool-bar-mode -1)
(size-indication-mode 1)
(add-hook 'find-file-hook 'linum-mode) ;; add line numbers to opened files
(auto-fill-mode -1)
(put 'set-goal-column 'disabled nil) ;; enable C-x C-n; disable C-u C-x C-n

;; Terminal
(let ((frame (framep (selected-frame))))
  (or (eq  t  frame)
      (eq 'pc frame)
      (define-key input-decode-map (kbd "C-[") [C-\[])
      (define-key input-decode-map "\C-i" [C-i])
      (define-key input-decode-map "\C-m" [C-m])
      (define-key input-decode-map "\C-j" [C-j])
     ))

;; remap defaults
(global-unset-key (kbd "C-z"))
;; remap ctl-x-map keys
(global-set-key (kbd "<menu>") ctl-x-map)
(define-key ctl-x-map (kbd "f") 'find-file)
(define-key ctl-x-map (kbd "s") 'save-buffer) ;; same as C-x C-s
(define-key ctl-x-map (kbd "w") 'kill-buffer-and-window)
(define-key ctl-x-map (kbd "x") 'revert-visible-windows)
(define-key ctl-x-map (kbd "<menu>") '(lambda () (interactive) (revert-buffer t t)))

;; Start up
(setq inhibit-startup-screen t
      initial-buffer-choice "~/Desktop/notes.md")

;; Mode line
(use-package smart-mode-line
  :ensure t
  :config 
  (sml/setup)
  (setq sml/theme 'light) ;; 'light, 'dark, 'respectful
  (setq sml/no-confirm-load-theme t)
  (setq sml/replacer-regexp-list nil)
  ;;(add-to-list 'sml/replacer-regexp-list '("^/sudo:root@.*:/" ":root:"))
  )

(use-package command-log-mode
  ;; (command-log-mode)
  ;; (clm/open-command-log-buffer)
  :ensure t)

;; scroll behaviour
(setq scroll-preserve-screen-position t)
(bind-key (kbd "<prior>") '(lambda () (interactive) (scroll-down-line 2)))
(bind-key (kbd "<next>") '(lambda () (interactive) (scroll-up-line 2)))

;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :bind (:map dockerfile-mode-map
			  ("<XF86Reply>" . dockerfile-build-buffer))
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  (setq dockerfile-mode-command "docker"))

(use-package docker
  :ensure t
  :bind (("C-c d" . docker)))

;;;;;;;;;;;;;;;;;;;;
;; CUSTOM MODES
;;;;;;;;;;;;;;;;;;;;

;; navigation mode

(define-minor-mode navi-mode
  "Toggle Navi Mode"
  :init-value nil
  :lighter " Navi"
  :group 'navi
  :keymap (let ((map (make-sparse-keymap)))
            (suppress-keymap map)

	    ;; Parenthesis movement
	    ;; C-M-u go up level
	    ;; C-M-n/p go next/previous paren on the same level
	    ;; C-M-e go to the end of defun
	    ;; C-M-a go to the start of defun
	    ;; C-m-f forward sexp
	    
            ;;(define-key map (kbd "s") 'set-goal-column)
            ;;(define-key map (kbd "S") '(lambda () (interactive) (set-goal-column 1)))

            ;; INSERT
            (define-key map (kbd "i") 'navi-mode)
            map))

(global-set-key (kbd "S-<return>") 'navi-mode)

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'display-buffer-alist
             '("^\\*.*\\*$" . (display-buffer-same-window)))

(use-package winner
  :ensure t
  ;; default keys C-c <arrow-key>
  :config (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(show-paren-mode 1)

(use-package electric
  :ensure t
  :config
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\}))))

;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
(defmacro alma/add-mode-pairs (hook pairs)
  `(add-hook ,hook
	     (lambda ()
	       (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
	       (setq-local electric--text-pairs electric-pair-pairs))))

(alma/add-mode-pairs 'shell-mode-hook '((?\' . ?\') (?\` . ?\`)))
(alma/add-mode-pairs 'sh-mode-hook '((?\' . ?\') (?\` . ?\`)))
(alma/add-mode-pairs 'markdown-mode-hook '((?\` . ?\`)))

(use-package company
  :ensure t
  :config 
  (add-hook 'after-init-hook 'global-company-mode)

  (add-to-list 'company-backends 'company-dabbrev-code)
  ;;(setq company-dabbrev-ignore-case 1)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-keywords)
  ;;(company-tng-configure-default)
  (setq company-selection-wrap-around t)

  :custom
  (completion-auto-help 'lazy)
  (company-begin-commands '(self-insert-command))
  (company-idle-delay  0.2)
  (company-minimum-prefix-legth 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (global-company-mode t)
  (company-require-match nil)
  )

(use-package undo-fu
  :ensure t
  :config
  :bind (("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo)
	 ("C-M-z" . undo-fu-only-redo-all)))

(use-package shell
  :bind (:map shell-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

(use-package browse-kill-ring
  :ensure t
  :config
  (setq browse-kill-ring-show-preview t)
  :bind (("M-y" . browse-kill-ring)))

;;;;;;;;;;;;;;;;;;;;
;; Navigating
;;;;;;;;;;;;;;;;;;;;

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1)
  ;;(setq key-chord-two-keys-delay .020
  ;;	key-chord-one-key-delay .020)

  ;; Add key-chord-mode to minor-mode-alist
  (if (not (assq 'key-chord-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(key-chord-mode " Chord ")
		  minor-mode-alist)))
  )

;; Least frequent bigram combinations
;;      gb gp
;;  jj  jc jf jg jh jk jl jm jp jq js jt jv jw jx jy jz
;;  qq  qb qf qg qh qk ql qm qp qt qv qw qx qy qz
;;  vv  vc vf vg vh vk vm vp vw vz
;;  ww  xb xd xg xk xm xs xw
;;  yy  zb zd zf zg zk zm zp zs zw zx

;;(key-chord-define-global k c)

(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(use-package ace-window
  :ensure t
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?w ?e ?r ?t)) ;; limit characters
  :bind (:map ctl-x-map
	 ("o" . ace-window)))


;;;;;;;;;;;;;;;;;;;;
;; Files

;; view same buffer with two windows
;; C-x 3 M-x follow-mode

;; Default C-c C-v prefix map for vlf-mode
;; prompt when opening large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(use-package direx
  :bind (
	 ("C-j" . direx:jump-to-directory)))

(add-to-list
 'directory-abbrev-alist
 '("^/jou" . "/mnt/mdbackup/journal"))

(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; otherwise tramp-mode will block emacs process
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;;(use-package ido
;;  :config (ido-mode 1)
;;  (setq ido-enable-flex-matching t
;;	ido-everywhere t
;;	ido-auto-merge-work-directories-length -1
;;    ido-use-virtual-buffers t)
;;  :bind (:map ctl-x-map
;;			  ("f" . ido-find-file)
;;			  ("b" . ido-switch-buffer)))


(use-package popwin
  :ensure t
  :config (popwin-mode 1)
  
  ;; setup kill-ring window
  (defun popwin-bkr:update-window-reference ()
  (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))

  (add-hook 'popwin:after-popup-hook 'popwin-bkr:update-window-reference)
  (push '("*Kill Ring*" :position bottom :height 20) popwin:special-display-config)

  ;; direx
  (push '(direx:direx-mode :position left :width 35 :dedicated t)
	popwin:special-display-config)
  
)

(use-package dired
  :delight "Dired "
  :custom
  (dired-auto-revert-buffer t) ;; reverts buffer on visit
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always)
  (delete-by-moving-to-trash t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
	      ("RET" . dired-find-alternate-file)
	      ("z" . open-in-external-app)
	      ("b" . (lambda () (interactive) (find-alternate-file "..")))))

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . smex)))

(use-package swiper
  :ensure t
  :bind (("M-'" . swiper-isearch)
	 :map isearch-mode-map
	 ("M-'" . avy-resume)))

(use-package avy
  :ensure
  :custom
  (avy-time-out-seconds 0.7)
  :bind (:map ctl-x-map
	      ("<menu>" . avy-goto-char-timer)
	      ;;("" . avy-goto-word-1)
	      ;;("j e" . avy-goto-line))
	      ))



;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;;;;;;;;;;;;;;;;;;;

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

;; Haskell
(use-package haskell-mode
  :config
  ;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-type 'cabal-repl))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "markdown")
  (setq markdown-indent-on-enter 'indent-and-new-item)
  :bind (:map markdown-mode-map
	      ("<return>" . markdown-custom-enter)
	      ("C-`" . markdown-insert-gfm-code-block)))


;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;


(defun my-cut ()
  "Cut the region, if no region cut current line"
  (interactive)
  (if (use-region-p)
      (kill-region 1 1 t)
    (kill-region (line-beginning-position) (line-end-position))))

(defun my-copy ()
  "Copy the region, if no region copy current line"
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill 1 1 t)
    (copy-region-as-kill (line-beginning-position) (line-end-position))))

(defun search-next-char (c)
  "Move cursor to the next character matched"
  (interactive "c")
  ;;(if (char-equal ?char-after ?c))
  (search-forward (char-to-string c) nil nil 1))

(defun sudo-save-buffer ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun revert-visible-windows ()
  "Revert visible unmodified windows"
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;;(message "file %s buffer %s" filename buf)
      (if (and filename (file-readable-p filename) (not (buffer-modified-p buf))
	       (get-buffer-window buf))
	  (progn (with-current-buffer buf
		   (revert-buffer :ignore-auto :nonconfirm :preserve-modes))
		 (message "Updated buffer %s" buf))
	;;(message "No visible window to update")
	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow buffer reverts to be undone

(defun my-revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Revert buffer from file in an undo-able manner."
  (interactive)
  (when (buffer-file-name)
    ;; Based upon `delphi-save-state':
    ;; Ensure that any buffer modifications do not have any side
    ;; effects beyond the actual content changes.
    (let ((buffer-read-only nil)
          (inhibit-read-only t)
          (before-change-functions nil)
          (after-change-functions nil))
      (unwind-protect
          (progn
            ;; Prevent triggering `ask-user-about-supersession-threat'
            (set-visited-file-modtime)
            ;; Kill buffer contents and insert from associated file.
            (widen)
            (kill-region (point-min) (point-max))
            (insert-file-contents (buffer-file-name))
            ;; Mark buffer as unmodified.
            (set-buffer-modified-p nil))))))

(defadvice ask-user-about-supersession-threat
  (around my-supersession-revert-buffer)
  "Use my-revert-buffer in place of revert-buffer."
  (let ((real-revert-buffer (symbol-function 'revert-buffer)))
    (fset 'revert-buffer 'my-revert-buffer)
    ;; Note that `ask-user-about-supersession-threat' calls
    ;; (signal 'file-supersession ...), so we need to handle
    ;; the error in order to restore revert-buffer.
    (unwind-protect
        ad-do-it
      (fset 'revert-buffer real-revert-buffer))))

(ad-activate 'ask-user-about-supersession-threat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open certain files in different applications

(defun open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath)) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown enter behaviour

(defun markdown-custom-enter ()
  "Markdown <enter> to promote list item if it is sub-list"
  (interactive)
  (if (null (markdown-cur-list-item-bounds))
	  (markdown-enter-key)
  (let ((indent (nth 2 (markdown-cur-list-item-bounds)))
		(list-end-pos (nth 1 (markdown-cur-list-item-bounds)))
		(list-begin-pos (nth 0 (markdown-cur-list-item-bounds)))
		(list-text-pos (nth 3 (markdown-cur-list-item-bounds))))
    (if (and
		 (> indent 0)
	     (= list-end-pos (+ list-begin-pos list-text-pos))
		 )
		(progn
		  ;;(message "promote list item")
		  (markdown-promote-list-item))
	  (progn
		;;(message "regular markdown enter key: line-end(%s)" (= list-end-pos (+ list-begin-pos list-end-pos)))
		(markdown-enter-key))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apt load bar
;; source: https://oremacs.com/2019/03/24/shell-apt/

(advice-add
 'ansi-color-apply-on-region
 :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the mode line instead."
  (let ((end-marker (copy-marker end))
        mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (let ((progress (buffer-substring-no-properties
                           (+ mb 2) (- (point) 2))))
            (delete-region mb (point))
            (ora-apt-progress-message progress)))))))
;; output in Echo Area
(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))
