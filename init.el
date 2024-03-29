;; TODO
;; https://realpython.com/emacs-the-best-python-editor/
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; Python LSP https://www.kotaweaver.com/blog/emacs-python-lsp/
;; https://github.com/howardabrams/dot-files/blob/master/emacs-python.org
;; https://github.com/bbatsov/projectile
;; links in shell-mode https://www.reddit.com/r/emacs/comments/g1fs0a/hyperlinks_in_shell/
;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/functions.el")
;; (setenv "BASH_ENV" "~/.bashrc")
;; (setq-default shell-command-switch "-c")
;; load same PATH as ~/.bashrc
;; (setenv "PATH" (shell-command-to-string "/bin/bash -i -c `/bin/echo -n $PATH`"))

(require 'package)

(setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stable" . "https://stable.melpa.org/packages/")
	    ("melpa" . "https://melpa.org/packages/"))
	  package-archive-priorities
	  '(("melpa-stable" . 10)
	    ("gnu" . 5)
	    ("melpa" . 0)))

;; Update signature manually
;; gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver keyserver.ubuntu.com --recv-keys 066DAFCB81E42C40
(package-initialize)
;;(setq package-check-signature  nil)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package))
  (require 'bind-key)) ;; required for :bind

(use-package diminish :ensure t)

;; Theme
(use-package humanoid-themes :ensure :defer)

(use-package circadian
  :ensure t
  :config
  (setq-default circadian-themes '(("8:00" . leuven)
                                   ("19:00" . tango-dark)))
  (circadian-setup)
  )

;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;

(setq-default ring-bell-function 'ignore ;; disable sound bell on error
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t 
              indent-tabs-mode nil
              tab-width 4
              tab-always-indent 'complete
              
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
(size-indication-mode 1)
(tool-bar-mode -1)
(global-visual-line-mode t)

(if (version< emacs-version "26")
  (progn
    (add-hook 'find-file-hook 'linum-mode) ;; add line numbers to opened files
    )
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

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode)
  )

;; remap defaults
(global-unset-key (kbd "C-z"))

;; remap ctl-x-map keys
;;(global-set-key (kbd "<menu>") ctl-x-map)
(define-key ctl-x-map (kbd "s") 'save-buffer) ;; same as C-x C-s

(define-key ctl-x-map (kbd "w") '(lambda () (interactive) (kill-buffer (buffer-name))))


;; scroll behaviour
(setq-default scroll-preserve-screen-position t)
(bind-key (kbd "<prior>") '(lambda () (interactive) (scroll-down-line 5)))
(bind-key (kbd "<next>") '(lambda () (interactive) (scroll-up-line 5)))
(bind-key (kbd "<prior>") '(lambda () (interactive) (scroll-down-line 5)))
(bind-key (kbd "<next>") '(lambda () (interactive) (scroll-up-line 5)))

;; Mode line
(use-package smart-mode-line
  :ensure t
  :config 
  (setq-default
   sml/theme 'dark ;; 'light, 'dark, 'respectful
   sml/no-confirm-load-theme t
   sml/replacer-regexp-list nil
   sml/no-confirm-load-theme t)
  ;;(add-to-list 'sml/replacer-regexp-list '("^/sudo:root@.*:/" ":root:"))
  (add-hook 'circadian-after-load-theme-hook 'sml/setup))

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
;; OS specific
;;;;;;;;;;;;;;;;;;;;

(cond ((eq system-type 'darwin)
       (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))))

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
            (define-key map (kbd "p") 'scroll-down-line)
            (define-key map (kbd "n") 'scroll-up-line)
            (define-key map (kbd "k") 'backward-up-list)
            (define-key map (kbd "j") 'down-list)
            (define-key map (kbd "h") 'backward-list)
            (define-key map (kbd "l") 'forward-list)
            ;;(define-key map (kbd "u") 'backward-sexp)
            ;;(define-key map (kbd "i") 'forward-sexp)
            map))

(global-set-key (kbd "S-<return>") 'navi-mode)

;; menu prefix mode

(define-prefix-command 'menu-prefix-map)
(let ((map 'menu-prefix-map))
  (define-key map (kbd "t") 'find-file-other-window)
  (define-key map (kbd "r") 'revert-current-or-shell-buffer)
  (define-key map (kbd "w") '(lambda () (interactive) (kill-buffer (buffer-name))))
  (define-key map (kbd "1") 'delete-other-windows)
  (define-key map (kbd "2") 'split-window-below)
  (define-key map (kbd "3") 'split-window-right)
  (define-key map (kbd "4") 'delete-window)
  ;(define-key map (kbd "t") 'counsel-recentf)
  (define-key map (kbd ",") 'beginning-of-buffer)
  (define-key map (kbd ".") 'end-of-buffer)
  )

(bind-key (kbd "<f9>") 'menu-prefix-map)

(bind-key [f5] 'save-buffer)

;;;;;;;;;;;;;;;;;;;;
;; Speciality MODES
;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind (:map menu-prefix-map
              ("g" . magit-status)))

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

;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(electric-pair-mode 1)
(electric-indent-mode 1)
(show-paren-mode 1)
(setq-default electric-indent-inhibit t)

(bind-key (kbd "c") 'copy-whole-line-at-cursor 'menu-prefix-map)

;;(use-package auto-complete :config  (ac-config-default))

(use-package company
  :ensure t
  :config
  (company-ac-setup)
  
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'haskell-mode-hook 'company-mode)
  ;; (add-hook 'haskell-interactive-mode-hook 'company-mode)
  ;;(setq company-backends '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))

  ;;(company-sort-by-occurrence)
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.25)
  :bind (:map company-active-map
              ("<up>" . (lambda () (interactive)
						  (company-complete-common-or-cycle -1)))
              ("<down>" . (lambda () (interactive)
						  (company-complete-common-or-cycle 1)))))

(use-package undo-fu
  :ensure t
  :config
  :bind (
         ;; ("C-z" . undo-fu-only-undo)
         ;; ("C-S-z" . undo-fu-only-redo)
         ;; ("C-M-z" . undo-fu-only-redo-all))
  ))

;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

;; Causes terminal to misinterpret commands such as <f8>
;; (global-set-key (kbd "M-[") 'backward-paragraph)
;; (global-set-key (kbd "M-]") 'forward-paragraph)

(bind-key (kbd "<home>") 'back-to-indentation-or-beginning-of-line)
(bind-key (kbd "<end>") 'end-of-buffer)

(use-package ace-window
  :ensure t
  :init (ace-window t)
  (setq-default aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r)) ;; limit characters
  :bind (:map ctl-x-map
              ("o" . ace-window)
              :map menu-prefix-map
              ("o" . ace-window)))

(use-package isearch-mode
  :bind (:map isearch-mode-map
              ("<up>" . isearch-repeat-backward)
              ("<down>" . isearch-repeat-forward)))

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

(use-package itail
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.log\\'" . itail-mode))
  (add-hook 'itail-mode-hook 'end-of-buffer)
  )


(require 'recentf)
(setq-default recentf-auto-cleanup 'never) ;; otherwise tramp-mode will block emacs process
(recentf-mode 1)
(setq-default recentf-max-menu-items 200
              recentf-max-saved-items 200)


(use-package ido
  :config
  (ido-mode 1)
  (setq-default ido-enable-flex-matching t
                ido-everywhere t
                ido-auto-merge-work-directories-length -1
                ido-use-virtual-buffers t
                ido-case-fold t)

  :bind (
         :map menu-prefix-map
              ("f" . ido-find-file)
              ("d" . ido-dired)
              ("b" . ido-switch-buffer)
         :map ctl-x-map
              ("f" . ido-find-file)
              ("b" . ido-switch-buffer)
              ("d" . ido-dired)))

(use-package counsel
  :ensure t
  :config 
  (setq-default ivy-use-virtual-buffers t
                enable-recursive-minibuffers t)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (setq-default mark-ring-max 100)
  :bind (
		 ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         :map menu-prefix-map
              ("x" . counsel-M-x)
              ("m" . counsel-mark-ring)))

(use-package swiper
  :ensure t
  :bind (:map menu-prefix-map
              ("s" . swiper)))

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
  :bind (("C-." . avy-goto-char-timer)
         :map menu-prefix-map
         ("SPC" . avy-goto-char-timer)))

(use-package docker-tramp
  :ensure t
  :config
   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package dockerfile-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
  :after (elpy)
  :config
  (when (require 'flycheck nil t)
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode))

  ;; allow cell delimeters to start not strictly from beginning of line
  (setq elpy-shell-codecell-beginning-regexp "\\(?:##.*\\|#\\s-*<codecell>\\|#\\s-*In\\[.*\\]:\\)\\s-*$"
		elpy-shell-cell-boundary-regexp "\\(?:##.*\\|#\\s-*<.+>\\|#\\s-*\\(?:In\\|Out\\)\\[.*\\]:\\)\\s-*$"))

;; SHELL

(use-package shell
  :config
  ;; Disable catching password prompt
  ;;(remove-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  
  (add-hook 'shell-mode-hook (lambda ()
							  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\') (?\` . ?\`))))
							  (setq-local electric-text-pairs electric-pair-pairs)))
  (setq explicit-shell-file-name "/bin/bash")
  :bind (
         ("C-c n" . new-shell)
         ("C-c r" . remote-ssh-shell)
         ("C-c t" . term)
         :map shell-mode-map
              ("<up>" . (lambda ()
                          (interactive)
                          (goto-char (point-max))
                          (comint-previous-input 1)))
              ("<down>" . (lambda ()
                            (interactive)
                            (goto-char (point-max))
                            (comint-next-input 1)))))

;; (use-package native-complete
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'shell
;;     (native-complete-setup-bash))
;;   (defun shell-mode-hook-setup ()
;;     "Set up `shell-mode'."
;;     (setq-local company-backends '((company-files company-native-complete)))
;;     ;; `company-native-complete' is better than `completion-at-point'
;;     (local-set-key (kbd "TAB") 'company-complete)

;;     ;; @see https://github.com/redguardtoo/emacs.d/issues/882
;;     (setq-local company-idle-delay 1)
;;     (company-mode)
;;     )
;;   (add-hook 'shell-mode-hook 'shell-mode-hook-setup)
;;   )

(use-package bash-completion
  :ensure t
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (setq bash-completion-use-separate-processes t))

;; Puppet
(use-package puppet-mode
  :ensure t)

;; Haskell

;;(require 'haskell-interactive-mode)
;; (require 'haskell-process
;;          :config
;;          (setq-default haskell-process-type 'stack-ghci))
;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html#Interactive-Haskell

;; (use-package lsp-mode :ensure t)
;; (use-package lsp-haskell
;;   :ensure
;;   :config (setq lsp-haskell-process-path-hie "hie-wrapper"))

;; (use-package lsp-ui  :ensure t)

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . (lambda ()
                           ;;(lsp)
                           ;;(direnv-update-environment)
                           (lsp-ui-doc-mode)
                           ;;(haskell-collapse-mode)
						   (interactive-haskell-mode)
						   (haskell-doc-mode))))
  :after (interactive-haskell-mode)
  :config
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines nil)
   '(haskell-process-auto-import-loaded-modules nil)
   '(haskell-process-log t)
   '(haskell-process-type 'cabal-repl) ;; 'cabal-repl or 'stack-ghci

   ;; cabal install hasktags (make sure `hasktags` in PATH)
   ;; M-x visit-tags-table (Manually select TAGS file)
   '(haskell-tags-on-save t) 
   )

  ;; cabal install hasktags
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
	(setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
	(add-to-list 'exec-path my-cabal-path))
  
  :bind (
		 :map haskell-mode-map
		 ("C-c C-l" . haskell-process-load-or-reload)
   		 ;;("C-c C-z" . haskell-interactive-switch)
         :map interactive-haskell-mode-map
		 ("M-." . haskell-mode-jump-to-def-or-tag)
         ;;("C-`" . haskell-interactive-bring)
         ("C-c f" . haskell-goto-first-error)
         :map haskell-cabal-mode-map
         ))

(use-package hindent
  :ensure t
  :config (add-hook 'haskell-mode-hook 'hindent-mode))

;; Markdown

(if (file-directory-p "~/.emacs.d/packages/markdown/")
	(progn
	  (add-to-list 'load-path "~/.emacs.d/packages/markdown/")
	  (load "markdown-mode")
	  ))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq-default markdown-command "markdown")

  ;; add bullet point on enter
  ;;(setq-default markdown-indent-on-enter 'indent-and-new-item)

  (add-hook 'markdown-mode-hook (lambda ()
							  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\` . ?\`))))
							  (setq-local electric-text-pairs electric-pair-pairs)))

  :bind (:map markdown-mode-map
              ("<return>" . markdown-custom-enter)
              ("C-`" . markdown-insert-gfm-code-block)))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" . yaml-mode))

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
  :config
  (setq-default url-user-agent ""))


;; Python

(use-package pyvenv
  ;; M-x pyvenv-activate RET <path-to-venv>
  :ensure t)

(use-package elpy
  :ensure t
  :init (elpy-enable)
  :config
  ;; pip install jupyter
  (cond
   ((executable-find "python") ;; use python instead of ipython to pick up virtualenv
    (setq-default python-shell-interpreter "python"
                  python-shell-interpreter-args "-i"))
   ((executable-find "jupyter")
    (setq-default python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))
   ))

(use-package auto-virtualenv
  :after (pyvenv)
  :ensure t
  :hook (python-mode . auto-virtualenv-set-virtualenv))

(use-package ein)

;; JavaScript

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook (lambda () (setq-local create-lockfiles nil)))
  (with-eval-after-load 'rjsx-mode
	(define-key rjsx-mode-map "<" nil)
	(define-key rjsx-mode-map (kbd "C-d") nil)
	(define-key rjsx-mode-map ">" nil)))

;; Latex

(use-package auctex
  :ensure
  :defer
  :config
  (setq-default TeX-auto-save t
				Tex-parse-self t
				TeX-master nil ;; ask for master file on a directory
				TeX-engine 'xetex
				TeX-PDF-mode t
				))

;; Terraform
(use-package terraform-mode
  :ensure t
  :hook
  (terraform-mode . company-mode))


