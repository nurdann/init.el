;; TO DO
;; remap C-y and M-w
;; cua C-c doesn't work in `C-s` and magit-mode
;; show mark-ring

;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/functions.el")
(setenv "BASH_ENV" "~/.bashrc")
(setq shell-command-switch "-ic")

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

(setq-default ring-bell-function 'ignore ;; disable sound bell on error
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t 
      indent-tabs-mode nil
      tab-width 4
      tab-always-indent nil
      electric-indent-mode 1

      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      set-mark-command-repeat-pop t ;; After C-u C-SPC, C-SPC cycles through the mark ring
      shift-select-mode t
      auto-compression-mode t)

(setq-default backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq-default custom-file "~/.emacs.d/custom.el")
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
(setq-default cua-delete-selection nil) ;; delete selection only with delete commands
(cua-mode t)

;; remap ctl-x-map keys
;;(global-set-key (kbd "<menu>") ctl-x-map)
(define-key ctl-x-map (kbd "f") 'find-file)
(define-key ctl-x-map (kbd "s") 'save-buffer) ;; same as C-x C-s

;; Start up
(setq-default inhibit-startup-screen t
      initial-buffer-choice "~/Desktop/notes.md")
(kill-buffer "*scratch*")

;; scroll behaviour
(setq-default scroll-preserve-screen-position t)
(bind-key (kbd "<prior>") '(lambda () (interactive) (scroll-down-line 5)))
(bind-key (kbd "<next>") '(lambda () (interactive) (scroll-up-line 5)))

;; Mode line
(use-package smart-mode-line
  :ensure t
  :config 
  (sml/setup)
  (setq-default
   sml/theme 'light ;; 'light, 'dark, 'respectful
   sml/no-confirm-load-theme t
   sml/replacer-regexp-list nil)
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
  (setq-default dockerfile-mode-command "docker"))

(use-package docker
  :bind (("C-c d" . docker)))

(use-package magit
  :ensure t
  :bind (:map ctl-x-map
	 ("g" . magit-status)))

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

;; menu prefix mode

(define-prefix-command 'menu-prefix-map)
(let ((map 'menu-prefix-map))
  (define-key map (kbd "f") 'find-file)
  (define-key map (kbd "t") 'find-file-other-window)
  (define-key map (kbd "r") 'revert-buffer-without-prompt)
  (define-key map (kbd "R") 'revert-visible-windows)
  (define-key map (kbd "w") '(lambda () (interactive) (kill-buffer (buffer-name))))
  ;; (define-key map (kbd "x") 'execute-extended-command)
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
  (define-key map (kbd "q") 'quoted-insert)
  (define-key map (kbd "s") 'save-buffer)
  (define-key map (kbd "t") 'recentf-open-files)
  ;; (define-key map (kbd "<left>") 'backward-sexp)
  ;; (define-key map (kbd "<right>") 'forward-sexp)
  ;; (define-key map (kbd "<up>") 'backward-up-list)
  ;; (define-key map (kbd "<down>") 'forward-list)
  )

(progn
  (bind-key [f5] 'previous-buffer)
   (bind-key [f6] 'next-buffer)
   )
(bind-key (kbd "<menu>") 'menu-prefix-map)

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

;; display buffers in same window
(customize-set-variable 'display-buffer-alist
			'(("^\\*shell.*\\*.*" . (display-buffer-same-window))
			  ("\\*Message\\*" . (display-buffer-same-window))))
(customize-set-variable 'Man-notify-method 'pushy)

(use-package winner
  ;; default keys C-c <arrow-key>
  :config (winner-mode 1))

(use-package minibuffer
  :config
  (setq-default resize-mini-windows t))


;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(bind-key (kbd "C-c C-k") 'alma/copy)

(use-package smex :ensure t)

(use-package auto-complete
  :ensure t
  :config  (ac-config-default))

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

(use-package undo-fu
  :ensure t
  :config
  :bind (:map cua--cua-keys-keymap
	 ("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo)
	 ("C-M-z" . undo-fu-only-redo-all)))

(use-package shell
  :bind (:map shell-mode-map
              ("<up>" . (lambda ()
			  (interactive)
			  (goto-char (point-max))
			  (comint-previous-input 1)))
              ("<down>" . (lambda ()
			    (interactive)
			    (goto-char (point-max))
			    (comint-next-input 1)))))

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
  (setq-default aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r)) ;; limit characters
  :bind (:map ctl-x-map
         ("o" . ace-window)
         :map menu-prefix-map
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
  :bind (:map menu-prefix-map
		("t" . treemacs-select-window)))

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
(setq-default auto-revert-remote-files 1) ;; enable in TRAMP mode

(require 'recentf)
(setq-default recentf-auto-cleanup 'never) ;; otherwise tramp-mode will block emacs process
(recentf-mode 1)
(setq-default recentf-max-menu-items 200
      recentf-max-saved-items 200)


(use-package ido
  :config (ido-mode 1)
  (setq-default ido-enable-flex-matching t
	ido-everywhere t
	ido-auto-merge-work-directories-length -1
	ido-use-virtual-buffers t)
  :bind (:map menu-prefix-map
	      ("f" . ido-find-file)
	      ("a" . ido-switch-buffer)
	      ("b" . ido-switch-buffer)
	      ("d" . ido-dired)))

(use-package counsel
  :ensure t
  :config 
  (setq-default ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (setq-default mark-ring-max 100)
  :bind (:map menu-prefix-map
	      ("x" . counsel-M-x)
	      ("m" . counsel-mark-ring)
	      ("k" . counsel-yank-pop)))

(use-package swiper
  :ensure t)

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
  :bind (:map menu-prefix-map
		("j" . avy-goto-char-timer)
		("l" . avy-goto-line)))

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

;; BASH

(alma/add-mode-pairs 'shell-mode-hook '((?\' . ?\') (?\` . ?\`)))
(alma/add-mode-pairs 'sh-mode-hook '((?\' . ?\') (?\` . ?\`)))


;; Haskell
;; https://gitlab.haskell.org/ghc/ghc/-/wikis/emacs#using-tags-to-quickly-locate-definitions-in-a-project
;; cabal install hasktags
;; hasktags --ignore-close-implementation .
;; M-x visit-tags-table

(use-package haskell-mode
  :ensure t
  :config
  ;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq-default haskell-font-lock-symbols t)
  (setq-default haskell-process-type 'cabal-repl))

;; Markdown

(add-to-list 'load-path "~/.emacs.d/packages/markdown/")
(load "markdown-mode")
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq-default markdown-command "markdown")
  (setq-default markdown-indent-on-enter 'indent-and-new-item)

  (alma/add-mode-pairs 'markdown-mode-hook '((?\` . ?\`)))
  :bind (:map markdown-mode-map
	      ("C-c C-k" . nil)
	      ("<return>" . markdown-custom-enter)
	      ("C-`" . markdown-insert-gfm-code-block)))

;; cmake

(use-package cmake-mode
  :ensure t
  :config
  (setq-default auto-mode-alist (append
			 '(("CMakeLists\\.txt\\'" . cmake-mode))
			 '(("\\.cmake\\'" . cmake-mode))
			 auto-mode-alist)))

;; mediawiki
(use-package mediawiki
  :ensure t
  :config
  (setq-default url-user-agent ""))
