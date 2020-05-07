;; TO DO
;; remap C-y and M-w
;; cua C-c doesn't work in `C-s` and magit-mode
;; show mark-ring

;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/functions.el")

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
  (setq key-chord-two-keys-delay .025
	key-chord-one-key-delay .025))

;; Theme
(use-package humanoid-themes :ensure :defer)

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . humanoid-light)
                           ("19:00" . humanoid-dark)))
  (circadian-setup))

;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;

(setq ring-bell-function 'ignore ;; disable sound bell on error
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t 
      indent-tabs-mode nil
      tab-width 4
      tab-always-indent 'complete
      electric-indent-mode 1

      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      set-mark-command-repeat-pop t ;; After C-u C-SPC, C-SPC cycles through the mark ring
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

;; load same PATH as ~/.bashrc
(setenv "PATH" (shell-command-to-string "/bin/bash -i -c `/bin/echo -n $PATH`"))

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
;; `C-s M-e C-v` to paste in isearch minibuffer
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

(if (file-directory-p "~/.emacs.d/packages/docker-tramp")
    (progn
      (add-to-list 'load-path "~/.emacs.d/packages/docker-tramp")
      (load "docker-tramp")))

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
  (define-key map (kbd "f") 'find-file)
  (define-key map (kbd "t") 'find-file-other-window)
  (define-key map (kbd "d") 'dired)
  (define-key map (kbd "r") 'revert-buffer-without-prompt)
  (define-key map (kbd "g") 'revert-visible-windows)
  (define-key map (kbd "w") '(lambda () (interactive) (kill-buffer (buffer-name))))
  (define-key map (kbd "e") 'eval-defun)
  (define-key map (kbd "E") 'eval-last-sexp)
  (define-key map (kbd "<menu>") 'execute-extended-command)
  (define-key map (kbd "<left>") 'previous-buffer)
  (define-key map (kbd "<right>") 'next-buffer)
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

;; display buffer in same window
;; (add-to-list 'display-buffer-alist '("^\\*.*\\*$" . (display-buffer-same-window)))

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
;; https://github.com/Fuco1/smartparens/wiki/Hybrid-S-expressions
;; https://github.com/Fuco1/smartparens/wiki/Permissions#insertion-specification
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode 1)
  (show-smartparens-global-mode t)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

  (sp-with-modes 'emacs-lisp-mode
    (sp-local-pair "'" nil :actions nil))

  (sp-with-modes 'haskell-mode
    (sp-local-pair "'" nil :actions nil))
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    ;;(sp-local-pair "<" nil :when '(lambda (id action context) (sp--looking-at-p "[[:space:]]")))
    )
  )

;; try
;; https://github.com/nivekuil/corral

(use-package company
  :ensure t
  :config ;;(add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode 1)
  ;; (setq company-global-modes '(not shell-mode))
  ;; company-capfs uses completion-at-point-functions
  ;; company-dabbrev-code uses words from current buffer
  ;; (add-to-list 'company-backends '(company-capf company-dabbrev-code))
  ;;(setq company-backends '((company-files company-keywords company-yasnippet) company-dabbrev-code (company-abbrev company-dabbrev)))
  ;;(setq company-backends '(company-capf company-files company-keywords company-dabbrev-code (company-dabbrev company-abbrev)))
  (setq company-backends '(company-capf company-files company-dabbrev-code company-dabbrev))
;;  (add-hook 'c++-mode-hook (lambda ()
;; 	(add-to-list (make-local-variable 'company-backends) 'company-irony)))


  ;; disable company mode in remote shell
  (add-hook 'shell-mode-hook '(lambda ()
                                (when (and (fboundp 'company-mode)
                                           (file-remote-p default-directory))
                                  (company-mode -1))))

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

;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows
(use-package buffer-move :ensure t
  :bind (("C-S-<up>" . 'buf-move-up)
         ("C-S-<down>" . 'buf-move-down)
         ("C-S-<left>" . 'buf-move-left)
         ("C-S-<right>" . 'buf-move-right)))

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(use-package ace-window
  :ensure t
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?w ?e ?r ?t)) ;; limit characters
  :bind (:map menu-prefix-map
	 ("s" . ace-window)))

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
(add-hook 'auto-revert-tail-mode-hook 'end-of-buffer)
(setq auto-revert-remote-files 1) ;; enable in TRAMP mode

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
  :bind (:map menu-prefix-map
	      ("a" . ido-switch-buffer)
	      ("b" . ido-switch-buffer)))

(use-package counsel
  :ensure t
  :config (counsel-mode 1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (setq mark-ring-max 100)
  :bind (:map menu-prefix-map
	      ("<menu>" . counsel-M-x)
	      ("f" . counsel-find-file)
	      ("m" . counsel-mark-ring)
	      ("k" . counsel-yank-pop)))

;;(use-package ivy
;;  :ensure t
;;  :bind (:map menu-prefix-map
;; 	 ("b" . ivy-switch-buffer)))


(use-package swiper
  :ensure t
  :chords (("sj" . swiper-isearch)))

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

;; Python

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook '(lambda () (add-to-list 'company-backends 'company-jedi))))

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
  ;;(eval-after-load 'company '(add-to-list 'company-backends 'company-irony)))

(add-hook 'c++-mode-hook '(lambda () 
   (add-to-list (make-local-variable 'company-backends) '(company-clang company-irony))) ;
   ;;(setq (make-local-variable 'company-backends) (delete 'company-semantic 'company-backends))
   ))

;; mediawiki
(use-package mediawiki
  :ensure t
  :config
  (setq url-user-agent ""))
