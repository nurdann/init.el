;; TO DO
;; remap C-y and M-w
;; show mark-ring

;; disable company-capf in remote shell-mode
;; 

;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;

(defun alma/cut ()
  "Cut the region, if no region cut current line"
  (interactive)
  (if (use-region-p)
      (kill-region 1 1 t)
    (kill-region (line-beginning-position) (line-end-position))))

(defun alma/copy ()
  "Copy the region, if no region copy current line"
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill 1 1 t)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (message "Copied current line")))
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


;;;;;;;;;;;;;;;;;;;;
;; add custom pairs

;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
(defun alma/add-mode-pairs (hook pairs)
  `(add-hook ,hook
	     (lambda ()
	       (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
	       (setq-local electric-text-pairs electric-pair-pairs))))

;;;;;;;;;;;;;;;;;;;;
;; show current file path

(defun copy-file-path ()
  "Copy file path of buffer to kill ring"
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;

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

(use-package diminish :ensure t)

;; Least frequent bigram combinations
;;      gb gp
;;  jj  jc jf jg jh jk jl jm jp jq js jt jv jw jx jy jz
;;  qq  qb qf qg qh qk ql qm qp qt qv qw qx qy qz
;;  vv  vc vf vg vh vk vm vp vw vz
;;  ww  xb xd xg xk xm xs xw
;;  yy  zb zd zf zg zk zm zp zs zw zx

(use-package use-package-chords
  :diminish key-chord-mode "Chord"
  :ensure t
  :config (key-chord-mode 1)
  (setq key-chord-two-keys-delay .020
	key-chord-one-key-delay .020))

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
      tab-width 4
      tab-always-indent nil
      electric-indent-mode 1

      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      set-mark-command-repeat-pop t ;; After C-u C-SPC, C-SPC cycles through the mark ring
      mark-ring-max 50

      window-combination-resize t
      shift-select-mode t
      auto-compression-mode t)

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

(if (version< emacs-version "26")
    (add-hook 'find-file-hook 'linum-mode) ;; add line numbers to opened files
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))
(column-number-mode 1) ;; show column position in mode line

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

;;use C-[zxcv] convention
(setq cua-delete-selection nil) ;; delete selection only with delete commands
(cua-mode t)

;; remap ctl-x-map keys
;;(global-set-key (kbd "<menu>") ctl-x-map)
(define-key ctl-x-map (kbd "f") 'find-file)
(define-key ctl-x-map (kbd "s") 'save-buffer) ;; same as C-x C-s

;; Start up
(setq inhibit-startup-screen t
      initial-buffer-choice "~/Desktop/notes.md")
(kill-buffer "*scratch*")

;; scroll behaviour
(setq scroll-preserve-screen-position t)
(bind-key (kbd "<prior>") '(lambda () (interactive) (scroll-down-line 5)))
(bind-key (kbd "<next>") '(lambda () (interactive) (scroll-up-line 5)))

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

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode 1)
  :custom (which-key-idle-delay 0.4) 
  	  (which-key-idle-secondary-delay 0.4))

;;;;;;;;;;;;;;;;;;;;
;; Speciality MODES
;;;;;;;;;;;;;;;;;;;;

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :bind (:map dockerfile-mode-map
	      ("C-c l" . dockerfile-build-buffer))
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  (setq dockerfile-mode-command "docker"))

(use-package docker
  :bind (("C-c d" . docker)))

(use-package magit
  :ensure t
  :chords (("gj" . magit-status)))

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
            (define-key map (kbd "i") 'navi-mode)
	    (define-key map (kbd "w") 'scroll-down-line)
	    (define-key map (kbd "s") 'scroll-up-line)
            map))

(global-set-key (kbd "S-<return>") 'navi-mode)

;; space prefix mode

(define-prefix-command 'menu-prefix-map)
(let ((map 'menu-prefix-map))
  (define-key map (kbd "f") 'ido-find-file)
  (define-key map (kbd "d") 'ido-dired)
  (define-key map (kbd "a") 'ido-switch-buffer)
  (define-key map (kbd "r") 'revert-without-query)
  (define-key map (kbd "g") 'revert-visible-windows)
  (define-key map (kbd "s") 'save-buffer)
  (define-key map (kbd "w") '(lambda () (interactive) (kill-buffer (buffer-name))))
  (define-key map (kbd "e") 'eval-defun)
  (define-key map (kbd "E") 'eval-last-sexp)
  (define-key map (kbd "q") 'kill-buffer-and-window)
  (define-key map (kbd "<menu>") 'smex)
  (define-key map (kbd "<left>") 'previous-buffer)
  (define-key map (kbd "<right>") 'next-buffer)
  (define-key map (kbd "o") 'ace-window)
  (define-key map (kbd "=") 'enlarge-window)
  (define-key map (kbd "-") 'shrink-window)
  (define-key map (kbd "[") 'shrink-window-horizontally)
  (define-key map (kbd "]") 'enlarge-window-horizontally)
  (define-key map (kbd "1") 'delete-other-windows)
  (define-key map (kbd "2") 'split-window-below)
  (define-key map (kbd "3") 'split-window-right)
  (define-key map (kbd "4") 'delete-window)
  )


(global-set-key (kbd "<menu>") 'menu-prefix-map)

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

;; display buffer
(add-to-list 'display-buffer-alist
             '("^\\*.*\\*$" . (display-buffer-same-window)))

(use-package winner
  ;; default keys C-c <arrow-key>
  :config (winner-mode 1))

(use-package minibuffer
  :config
  (setq resize-mini-windows t))


;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(bind-key (kbd "C-c C-k") 'alma/copy)

(use-package smex :ensure t)

;;;;;;;;;;;;;;;;;;;;
;; Smart parentheses

;; https://smartparens.readthedocs.io/en/latest/pair-management.html
;; https://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode 1)
  (show-smartparens-global-mode t)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "'" nil :actions nil))

  (sp-with-modes 'haskell-mode
    (sp-local-pair "'" nil :actions nil))
  )

(use-package company
  :ensure t
  :config ;;(add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode 1)

  ;; company-capfs uses completion-at-point-functions
  ;; company-dabbrev-code uses words from current buffer
  ;; (add-to-list 'company-backends '(company-capf company-dabbrev-code))

  :custom
  (company-selection-wrap-around t)
  (company-begin-commands '(self-insert-command))
  (company-idle-delay  0.2)
  (company-minimum-prefix-legth 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil))

(use-package undo-fu
  :ensure t
  :config
  :bind (:map cua--cua-keys-keymap
	 ("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo)
	 ("C-M-z" . undo-fu-only-redo-all)))

(use-package shell
  :bind (:map shell-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

(use-package popwin
  :ensure t
  :config (popwin-mode 1)
  ;; setup kill-ring window	
  (defun popwin-bkr:update-window-reference ()
    (popwin:update-window-reference 'browse-kill-ring-original-window :safe t))
  (add-hook 'popwin:afterp-opup-hook 'popwin-bkr:update-window-reference)
  (push '("*Kill Ring*" :position bottom :height 20) popwin:special-display-config)

  (bind-key (kbd "C-;") popwin:keymap))

(use-package browse-kill-ring
  :ensure t
  :config  (setq browse-kill-ring-show-preview t)
  :bind (:map cua--cua-keys-keymap
	 ("M-v" . browse-kill-ring)
	 :map browse-kill-ring-mode-map
	 ("N" . browse-kill-ring-forward)
	 ("P" . browse-kill-ring-previous)))

;;(add-to-list 'load-path "~/.emacs.d/packages/show-marks")
;;(require 'show-marks)
;;(global-set-key (kbd "C-c m") 'show-marks)
;;(x-popup-menu '((0 0) init.el) '(menu hi bye))

;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(use-package ace-window
  :ensure t
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?w ?e ?r ?t)) ;; limit characters
  :bind (:map ctl-x-map
	 ("o" . ace-window)))

(use-package treemacs
  :ensure t
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-follow-after-init)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-width 35)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :chords (("tj" . treemacs-select-window)))

;;;;;;;;;;;;;;;;;;;;
;; Files

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
		'("\\.zip\\'" ".zip" "unzip")))

;; view same buffer with two windows
;; C-x 3 M-x follow-mode

;; Default C-c C-v prefix map for vlf-mode
;; prompt when opening large files
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; otherwise tramp-mode will block emacs process
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 50)
(bind-key (kbd "C-x M-f") 'recentf-open-files)

(use-package ido
  :config (ido-mode 1)
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-auto-merge-work-directories-length -1
	ido-use-virtual-buffers t)
  :bind (:map ctl-x-map
	      ("f" . ido-find-file)
	      ("b" . ido-switch-buffer)))

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

(use-package swiper
  :ensure t
  :bind (("M-'" . swiper-isearch))
  :chords (("sj" . swiper-isearch)))

(use-package avy
  :ensure
  :custom
  (avy-time-out-seconds 0.7)
  :bind (("C-'" . avy-goto-char-timer)
	 ("C-\"" . avy-goto-line))
  :chords (("jf" . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t)


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

;; BASH

(alma/add-mode-pairs 'shell-mode-hook '((?\' . ?\') (?\` . ?\`)))
(alma/add-mode-pairs 'sh-mode-hook '((?\' . ?\') (?\` . ?\`)))


;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  ;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-type 'cabal-repl))

;; Markdown

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "markdown")
  (setq markdown-indent-on-enter 'indent-and-new-item)

  (alma/add-mode-pairs 'markdown-mode-hook '((?\` . ?\`)))
  :bind (:map markdown-mode-map
	      ("C-c C-k" . nil)
	      ("<return>" . markdown-custom-enter)
	      ("C-`" . markdown-insert-gfm-code-block)))

;; cmake

(use-package cmake-mode
  :ensure t
  :config
  (setq auto-mode-alist (append
			 '(("CMakeLists\\.txt\\'" . cmake-mode))
			 '(("\\.cmake\\'" . cmake-mode))
			 auto-mode-alist))
  (add-hook 'cmake-mode-hook '(add-to-list 'company-backends 'company-cmake)))

;; C++

(defun company-rtags-setup ()
  "Configure company-backend for company-rtags"
  (delete 'company-semantic company-backends)
  (setq rtags-completions-enabled t)
  (push '(company-rtags :with company-yasnippet) company-backends))

;;(use-package rtags
;;  :ensure t
;;  :config
;;  (rtags-enable-standard-keybindings)
;;  (setq rtags-autostart-diagnostics t)
;;  (rtags-diagnostics)
;;  (rtags-start-process-unless-running)
;;  (add-hook 'c++-mode-hook 'company-rtags-setup))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-c-headers :ensure t)

(use-package company-irony :ensure t
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))

(add-hook 'c++-mode-hook '(lambda () 
   (add-to-list (make-local-variable 'company-backends) 'company-clang)
   ;;(setq (make-local-variable 'company-backends) (delete 'company-semantic 'company-backends))
   ))

(add-hook 'c++-mode-hook (lambda ()
			   (local-set-key (kbd "<tab>") 'company-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company backend for IPA symbols


;; PARSE CSV into list, e.g.
;; a, b \n f, c, e -> ((a, b) (f c e))
;; https://gist.github.com/syohex/5487731
(defun parse-csv-file (file)
  (interactive
   (list (read-file-name "CSV file: ")))
  (let ((buf (find-file-noselect file))
        (result nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (push (split-string line ",") result))
        (forward-line 1)))
    (reverse result)))

(defun company-ipa-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-ipa-backend))
    (prefix (and (eq major-mode 'fundamental-mode)
		 (company-grab-symbol)))
    (candidates

	  (dolist (element ipa-completions)
		(let ((head (car element)))
		   (if (string= head arg)	
			   (return (cdr element)))))
	  )))

(let ((ipa-symbols "~/.emacs.d/dictionary/ipa-symbols.csv"))
  (if (file-exists-p ipa-symbols)
      (progn
	(defconst ipa-completions (parse-csv-file ipa-symbols))
	(add-to-list 'company-backends 'company-ipa-backend))
    (message "IPA table of symbols not found")))

