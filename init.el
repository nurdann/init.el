;; -*- lexical-binding: t -*-

;; TODO
;; Paste links to org mode, markdown
;; Copy cli outputs
;; Treesitter with Python
;; puni-mode https://github.com/AmaiKinono/puni
;; Replicate launch.json for sending code to buffer
;; https://github.com/bbatsov/projectile
;; https://github.com/riscy/shx-for-emacs/blob/master/shx.el
;; [ ] https://sideshowcoder.com/2020/04/16/emacs-markdown-list-dwim/

;;;;;;;;;;;;;;;;;;;;
;;; Install Emacs https://www.emacswiki.org/emacs/BuildingEmacs
;;; make sure prefix path is same on host machine
;; docker run -ti -v ~/emacs:${HOME}/emacs -v ~/.local/bin:/emacs-bin ubuntu
;; apt install wget make gcc libgnutls28-dev pkg-config libncurses-dev
;; wget https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.gz
;; tar -xf emacs-27.2.tar.gz
;; cd emacs-27.2
;; ./configure --prefix=/home/<users>/emacs --bindir=/emacs-bin
;; make
;; ./src/emacs -q # check emacs works
;; make install
;; make uninstall # to remove it


;;
;; How to debug elisp https://whatacold.io/blog/2022-07-17-emacs-elisp-debug/

;;;;;;;;;;;;;;;;;;;;
;; INIT
;;;;;;;;;;;;;;;;;;;;

(customize-set-variable 'custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;; To update gpg keyring for Melpa and Elpa
;; If Ubuntu key server ever goes down, then download keyring from https://ftp.gnu.org/gnu/gnu-keyring.gpg
;; source: https://stackoverflow.com/a/5710891/1374078
;; (shell-command-to-string "gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver keyserver.ubuntu.com --recv-keys 066DAFCB81E42C40")

;; Bootstrap `use-package'
(require 'package)
(setq package-archives
	  '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stable" . "https://stable.melpa.org/packages/")
	    ("melpa" . "https://melpa.org/packages/"))
	  package-archive-priorities
	  '(("melpa-stable" . 10)
	    ("gnu" . 5)
	    ("melpa" . 20)))

(package-initialize)

(unless (package-installed-p 'use-package)  
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile 
  (require 'use-package))

;; Install all packages
(use-package use-package-ensure
  :ensure nil
  :custom (use-package-always-ensure nil))

;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;

(setq-default ring-bell-function 'ignore ;; disable sound bell on error
              ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Special-Isearch.html#Special-Isearch
              read-buffer-completion-ignore-case t
              read-file-name-completion-ignore-case t
              case-fold-search t
              case-replace t
              isearch-case-fold-search nil
              
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

;;;;;;;;;;;;;;;;;;;;
;; Quality of life

(use-package diminish)

(use-package clipetty
  :hook (after-init . global-clipetty-mode)
  )

;; remap defaults
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "s-t"))

;; remap ctl-x-map keys
;;(global-set-key (kbd "<menu>") ctl-x-map)
(bind-key (kbd "C-k") 'kill-buffer ctl-x-map)

;; minibuffer rebinds
(define-key minibuffer-local-map (kbd "<up>") 'previous-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-history-element)

;; switching buffers
(bind-key (kbd "<f5>") 'previous-buffer)
(bind-key (kbd "<f6>") 'next-buffer)
(bind-key (kbd "<f7>") 'switch-to-buffer)
(bind-key (kbd "C-b") 'switch-to-buffer)
(bind-key (kbd "C-f") 'find-file)

;; scroll behaviour
(setq-default scroll-preserve-screen-position t)
(bind-key (kbd "<prior>") '(lambda () (interactive) (scroll-down 5)))
(bind-key (kbd "<next>") '(lambda () (interactive) (scroll-up 5)))


(require 'recentf)
(setq-default recentf-auto-cleanup 'never) ;; otherwise tramp-mode will block emacs process
(recentf-mode 1)
(setq-default recentf-max-menu-items 200
              recentf-max-saved-items 200)

;; Save minibuffer history
(savehist-mode)

;; Save buffer when going out of focus
(progn
  (defadvice switch-to-buffer (before save-buffer-now activate)
    (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
  (defadvice other-window (before other-window-now activate)
    (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
  (defadvice ace-window (before ace-window-now activate)
    (when (and buffer-file-name (buffer-modified-p)) (save-buffer)))
)

;; Theme

(load-theme 'tsdh-light t)

;; Mode line
(use-package smart-mode-line
  :config 
  (setq-default
   sml/theme 'dark ;; 'light, 'dark, 'respectful
   sml/no-confirm-load-theme t
   sml/replacer-regexp-list nil
   sml/no-confirm-load-theme t)
  ;;(add-to-list 'sml/replacer-regexp-list '("^/sudo:root@.*:/" ":root:"))
  (sml/setup)
  )

(use-package which-key
  :config (which-key-mode 1)
  :custom (which-key-idle-delay 0.4) 
  (which-key-idle-secondary-delay 0.4))

(use-package desktop
  :ensure nil
  :if (display-graphic-p)
  :custom
  (desktop-path '("~/.emacs.d/"))
  :config
  (desktop-save-mode 1))

(add-hook 'text-mode-hook 'goto-address-mode)
(add-hook 'prog-mode-hook 'goto-address-mode)

(use-package f) ;; provides `f-' functions for file manipulation

(use-package highlight-indent-guides
  :hook
  (yaml-mode . highlight-indent-guides-mode)
  (python-mode . highlight-indent-guides-mode)
  :config
   (setq highlight-indent-guides-auto-odd-face-perc 5)
   (setq highlight-indent-guides-auto-even-face-perc 15)
   (setq highlight-indent-guides-auto-character-face-perc 20)
  )

;;;;;
;; A single long line can make Emacs unrespnsive...

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-display-reordering nil)
(setq-default bidi-inhibit-bpa t)

;;;;;;;;;;;;;;;;;;;;
;; Performance
;;;;;;;;;;;;;;;;;;;;

;; When opening file with long line
;; instead of trying to tweak deep Emacs functionalities
;; use M-x `find-file-literally'
;; (setq-default bidi-paragraph-direction 'left-to-right)
;; (if (version<= "27.1" emacs-version)
;;     (setq bidi-inhibit-bpa t))

;;;;;;;;;;;;;;;;;;;;
;; OS specific
;;;;;;;;;;;;;;;;;;;;

(cond ((eq system-type 'darwin)
       ;; If GUI emacs launched, then environment variables are different than from shell
       (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
       (add-to-list 'exec-path "/usr/local/bin"))
      )

;; Make command interactive to load environment variables
;; However, it results in error
;; bash: cannot set terminal process group (-1): Inappropriate ioctl for device
;;(setq-default shell-command-switch "-ic")

;;;;;;;;;;;;;;;;;;;;
;; CUSTOM MODES
;;;;;;;;;;;;;;;;;;;;

;; navigation mode

(defvar navi-mode-map
  (let ((map (make-sparse-keymap)))
            (suppress-keymap map)

            (define-key map (kbd "i") 'navi-mode)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "h") 'left-char)
            (define-key map (kbd "l") 'right-char)
            
            (define-key map (kbd "SPC") 'set-mark-command)
            (define-key map (kbd "f") 'forward-sexp)
            (define-key map (kbd "b") 'backward-sexp)
            (define-key map (kbd "u") 'backward-up-list)
            (define-key map (kbd "d") 'down-list)

            (define-key map (kbd "p") (kbd "C-c C-p"))
            (define-key map (kbd "n") (kbd "C-c C-n"))

            (define-key map (kbd "c") 'kill-ring-save)
            (define-key map (kbd "x") 'kill-region)
            map))

(define-minor-mode navi-mode
  "Toggle Navi Mode"
  :init-value nil
  :lighter " Navi"
  :group 'navi
  :mode-map navi-mode-map)
;;(unintern 'navi-mode-map)
(global-set-key (kbd "<f1>") 'navi-mode)

;; menu prefix mode

(define-prefix-command 'menu-prefix-map)
(let ((map 'menu-prefix-map))
  (define-key map (kbd "r") 'u/revert-current-buffer-or-visible-windows)
  (define-key map (kbd "w") '(lambda () (interactive) (kill-buffer (buffer-name))))
  (define-key map (kbd "f") 'find-file)
  (define-key map (kbd "b") 'switch-to-buffer)
  (define-key map (kbd "c") 'u/copy-command-output)
  (define-key map (kbd "s") 'save-buffer)
  )

(bind-key (kbd "<f12>") 'menu-prefix-map)

(use-package hydra)

;; hydra
(unless (version< emacs-version "28")
  (defhydra hydra-window (global-map "C-x o" :color red)
    "Move between windows (i.e. panes)"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("o" other-window)
    ("q" nil)
    ))


(defun my-hydra-3 ()
  "Conditional hyrda https://kitchingroup.cheme.cmu.edu/blog/2015/06/24/Conditional-hydra-menus/ "
  (interactive)
  (let ((conditionals
         `(((evenp (line-number-at-pos)) . ("e" (message-box "Even second") ,(format "Even: %s" (line-number-at-pos))))
           ((oddp (line-number-at-pos)) . ("o" (message-box "Odd second") ,(format "Odd: %s" (line-number-at-pos))))
           (t . ("a" (message-box "always true") "always"))
           (nil . ("n" (message-box "never") "never")))))
    (eval
     (append
      '(defhydra my-hydra-3 (:color blue) "My hydra")
      (loop for cond in conditionals
            if (eval (car  cond))
            collect (cdr cond))))
    (my-hydra-3/body)))


;;;;;;;;;;;;;;;;;;;;
;; Speciality MODES
;;;;;;;;;;;;;;;;;;;;

;; TODO Hydra navigation mode https://github.com/abo-abo/hydra/wiki/Window-Management
;; https://www.reddit.com/r/emacs/comments/b13n39/comment/eik6xzb/?utm_source=share&utm_medium=web2x&context=3

(use-package magit
  :custom
  (magit-section-initial-visibility-alist (quote ((untracked . hide))))
  (magit-auto-revert-mode nil)
  (magit-save-repository-buffers nil)
  :bind ())

(use-package smerge-mode
  :ensure nil
  :config
  (setq-default smerge-command-prefix (kbd "C-c ^"))
  :bind (:map smerge-mode-map
         ("n" . smerge-next)
         ("p" . smerge-prev)
         ("RET" . smerge-keep-current)
         ("m" . smerge-keep-mine)
         ("o" . smerge-keep-other)
         ("a" . smerge-keep-all)
         ))

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

;; display buffers in same window
(customize-set-variable 'display-buffer-alist
                        '(("^\\*shell.*\\*.*" . (display-buffer-same-window))
                          ("\\*Message\\*" . (display-buffer-same-window))
                          ("\\*Python\\*" . (display-buffer-same-window))
                          ))
(customize-set-variable 'Man-notify-method 'pushy)

(use-package winner
  :ensure nil
  ;; default keys C-c <arrow-key>
  :config (winner-mode 1))

;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(electric-pair-mode 1)
(electric-indent-mode -1)
(show-paren-mode 1)
(setq-default electric-indent-inhibit t)

;;(bind-key (kbd "c") 'copy-whole-line-at-cursor 'menu-prefix-map)

(use-package undo-fu
  :config
  :bind (
         "C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)
         ("M-z" . undo-fu-only-redo)
         ("C-M-z" . undo-fu-only-redo-all))

;;;;;;;;;;;;;;;;;;;;
;; Navigating 
;;;;;;;;;;;;;;;;;;;;

;(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

;; Causes terminal to misinterpret commands such as <f8>
;; (global-set-key (kbd "M-[") 'backward-paragraph)
;; (global-set-key (kbd "M-]") 'forward-paragraph)

(bind-key (kbd "<home>") 'back-to-indentation-or-beginning-of-line)
(bind-key (kbd "<end>") 'end-of-visual-line-or-end-of-line)

(use-package ace-window  
  :config (ace-window t)
  (setq-default aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r)) ;; limit characters
  :bind (
         ("C-o" . ace-window)
         ))

(use-package ace-jump-mode
  :bind (:map menu-prefix-map
              ("u" . ace-jump-http-mode)
              ))


(use-package pcre2el)

(defun ace-jump-http-mode ()
  "Jump to URLs"
  (interactive)
  (ace-jump-do "https?://[#%+.0-:=@-Z_a-z~-]\\{1,256\\}\\.[()0-9A-Za-z]\\{1,6\\}\\b\\([#%&()+.-:=?-Z_a-z~-]*\\)"))

(defun ace-jump-file-mode ()
  "Jump to URLs"
  (interactive)
  (ace-jump-do "https?://[#%+.0-:=@-Z_a-z~-]\\{1,256\\}\\.[()0-9A-Za-z]\\{1,6\\}\\b\\([#%&()+.-:=?-Z_a-z~-]*\\)"))

(use-package isearch-mode
  :ensure nil
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
  :config
  (require 'vlf-setup)
  (setq-default vlf-batch-size 10000))

(when (version< emacs-version "27.1")
  (ido-mode t)
  )
(unless (version< emacs-version "27")
  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles orderless basic partial-completion))))
    )

  (use-package consult
    :custom
    (consult-preview-key 'any)
    :after (orderless vertico bash-completion)
    :init
    (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
    :config
    (consult-customize consult-completion-in-region
                       :completion-styles '(substring orderless basic))
    :bind (
           ("C-b" . consult-buffer)
           ("M-y" . consult-yank-pop)
           :map menu-prefix-map
           ("d" . consult-line)
           ))


  (use-package vertico
    ; https://kristofferbalintona.me/posts/202202211546/#vertico
    :custom
    (vertico-count 10)
    (vertico-cycle t)
    :init
    (vertico-mode)
    :bind (:map vertico-map
                ("<down>" . vertico-next)
                ("<up>" . vertico-previous)
                ("DEL" . vertico-directory-delete-word)
                ))
  )

(use-package dired
  :ensure nil
  :delight "Dired "
  :custom
  (dired-auto-revert-buffer t) ;; reverts buffer on visit
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("z" . open-in-external-app)
              ("b" . (lambda () (interactive) (find-alternate-file "..")))))

(use-package avy
  :custom
  (avy-time-out-seconds 0.7)
  :bind (
         ("C-_" . avy-goto-char-timer) ;; `C-/' translates to `C-/' in terminal
         ("C-/" . avy-goto-char-timer)))

(use-package dockerfile-mode)

(use-package tramp
  :after (embark)
  :config
  ;; https://www.emacswiki.org/emacs/TrampAndDocker
  (push ;; /docker:host:~/
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)

  (defun add-ssh-agent-to-tramp ()
    (cl-pushnew '("-A")
                (cadr (assoc 'tramp-login-args
                             ;; replace `ssh' with `plink' if using Putty
                             (assoc "ssh" tramp-methods)))
                :test #'equal))
  (add-ssh-agent-to-tramp)
  :bind (
         :map embark-file-map
         ("S" . add-sudo-to-path)))

;;;;;;;;;;;;;;;;;;;;
;; Completion


;;;;;;;;;;;;;;;;;;;;
;; Monitoring
;;;;;;;;;;;;;;;;;;;;

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;;;;;;;;;
;; Language modes
;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :config
  )
;; SHELL

(use-package shell
  :ensure nil
  :config
  ;; Disable catching password prompt
  ;;(remove-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  
  (add-hook 'shell-mode-hook (lambda ()
							   (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\') (?\` . ?\`))))
							   (setq-local electric-text-pairs electric-pair-pairs)))
  (add-hook 'shell-mode-hook (lambda ()
                               (setenv "PAGER" "cat")
                               (process-send-string (get-buffer-process (current-buffer))
                                                    "export PAGER=cat\n")))
  
  (setq explicit-shell-file-name "/bin/bash")
  (shell-dirtrack-mode -1)
  (dirtrack-mode 1)
  :bind (
         ("C-c n" . new-shell)
         :map shell-mode-map
         ("C-c c" . copy-previous-shell-command-output)
         ("M-p" . (lambda ()
                     (interactive)
                     (goto-char (point-max))
                     (comint-previous-input 1)))
         ("M-n" . (lambda ()
                       (interactive)
                       (goto-char (point-max))
                       (comint-next-input 1)))))

(when (version<= "27.1" emacs-version)
  (use-package embark 
    :config
    :bind (
           ("<f11>" . embark-act)
           :map embark-file-map
           ("s" . open-shell-in-directory)
           ))

  (use-package embark-consult
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))
    )

(use-package bash-completion
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (setq bash-completion-use-separate-processes t))

;; Puppet
(use-package puppet-mode)

;; Haskell

;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html#Interactive-Haskell

(use-package haskell-mode
  :hook ((haskell-mode
          . (lambda ()
              (haskell-indentation-mode -1)
			  (interactive-haskell-mode)
              
              (setq-local
               tab-stop-list '(2 4)
               indent-line-function 'indent-relative
               tab-width 2
               )

              )))
  :custom
  (haskell-process-load-or-reload t)
  (haskell-process-suggest-remove-import-lines nil)
  (haskell-process-auto-import-loaded-modules nil)
  (haskell-process-log t)
  (haskell-interactive-popup-errors nil)
  (haskell-process-type 'cabal-repl) ;; 'cabal-repl or 'stack-ghci
  ;; cabal install hasktags (make sure `hasktags` in PATH)
  ;; M-x visit-tags-table (Manually select TAGS file)
  ;;(haskell-tags-on-save t)
  ;;(haskell-process-path-cabal "cabal")
  :config

  ;; cabal install hasktags
  ;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  ;;   (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  ;;   (add-to-list 'exec-path my-cabal-path))
    
  )

(use-package hindent
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
							  (setq-local electric-text-pairs electric-pair-pairs)

                              (markdown-toggle-url-hiding t)))

  :bind (:map markdown-mode-map
              ("<return>" . markdown-custom-enter)
              ("C-`" . markdown-insert-gfm-code-block)))


;; Org mode
(use-package org-mode
  :ensure nil
  :custom
  (org-export-with-sub-superscripts nil)
  (org-fontify-emphasized-text nil)
  (org-startup-folded t)
  (org-log-done t)
  (org-agenda-files (list "~/Documents/notes/notes.org"))
  (org-export-backend '(ascii html icalendar latex odt md))
  :config
  ;(setq org-agenda-files (list "~/Documents/notes/notes.org"))
  (setq org-agenda-custom-commands
   '(("1" "Q1" tags-todo "+important+urgent")
     ("2" "Q2" tags-todo "+important-urgent")
     ("3" "Q3" tags-todo "-important+urgent")
     ("4" "Q4" tags-todo "-important-urgent")))
  :bind (:map org-mode-map
              ("RET" . org-insert-list-item-on-enter)
              ("<backtab>" . org-global-cycle)
              ("C-c t" . u/insert-current-hour)
         :map menu-prefix-map
              ("j d" . (lambda ()
                         (interactive)
                         (insert (format-time-string "%H:%M"))))
              )
  )

;; Org-roam
;; source https://www.orgroam.com/manual.html#Introduction
(when (version<= "26.1" emacs-version)
    (use-package org-roam
      :if (eq system-type 'darwin)
      :custom
      (make-directory "~/Documents/notes/org-roam" t)
      (org-roam-complete-everywhere t)
      (org-roam-database-connector 'sqlite-builtin)
      :config
      (setq org-roam-directory (file-truename "~/Documents/notes/org-roam"))
      (org-roam-db-autosync-mode)
      :bind (:map menu-prefix-map
                  ("n l" . org-roam-buffer-toggle)
                  ("n f" . org-roam-node-find)
                  ("n i" . org-roam-node-insert)
                  ("n l" . org-roam-buffer-toggle)
                  ("n g" . org-roam-graph)
                  ("n c" . org-roam-capture)
                  ("n j" . org-roam-dailies-capture-today)
                  :map org-mode-map
                  ("C-M-i" . completion-at-point)
                  ("S-<up>" . nil)
                  ("S-<down>" . nil)
                  ))

  ;; resources: https://bastibe.de/2018-04-02-scheduling-future-todos-in-org-journal.html
  (use-package org-journal
    :custom
    (org-journal-dir "~/Documents/notes/org-journal-yearly")
    (org-journal-file-type 'yearly)
    (org-journal-enable-agenda-integration t)
    (org-journal-time-format "")
    (org-journal-hide-entries-p t)
    (org-journal-date-format "%A, %d/%m/%Y")
    :bind (:map menu-prefix-map
           ("j f" . u/org-journal-new-entry)
           ("j t" . u/org-journal-new-todo)
           ("j c" . org-journal-new-scheduled-entry)
           :map org-journal-mode-map
           ("<f1>" . u/org-journal-new-entry)
           ("<f2>" . u/org-journal-new-todo)
           )
    )
  )

;; YAML
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;; cmake

(use-package cmake-mode
  :config
  (setq-default auto-mode-alist (append
                                 '(("CMakeLists\\.txt\\'" . cmake-mode))
                                 '(("\\.cmake\\'" . cmake-mode))
                                 auto-mode-alist)))

(use-package cc-mode
  :ensure nil
  :config
  ;; potentionally look at https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
  (setq-default auto-mode-alist (append
                                 '(("\\.ebpf\\'" . c-mode))
                                 auto-mode-alist)))

;; mediawiki
(use-package mediawiki
  :if (require 'mediawiki nil 'noerror)
  :config
  (setq-default url-user-agent ""))


;; Python

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py?\\'"
)

(use-package python
  :ensure nil
  :custom
  (py-keep-windows-configuration nil)
  (python-indent-guess-indent-offset nil)
  ;; remove warnings
  (python-shell-completion-native-enable nil)
  :bind
  (
   :map python-mode-map
   ("C-c C-p" . u/run-python)
   ("C-c c" . u/send-file-to-python-shell)
   ("RET" . electric-newline-and-maybe-indent)
   :map inferior-python-mode-map
   ))

(use-package pyvenv
  ;; M-x pyvenv-activate RET <path-to-venv>
  )
 
;; Rust

(use-package rust-mode
  :no-require
  )

;; JavaScript

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook (lambda () (setq-local create-lockfiles nil)))
  (with-eval-after-load 'rjsx-mode
	(define-key rjsx-mode-map "<" nil)
	(define-key rjsx-mode-map (kbd "C-d") nil)
	(define-key rjsx-mode-map ">" nil)))

;; Latex

(use-package auctex
  :defer
  :config
  (setq-default TeX-auto-save t
				Tex-parse-self t
				TeX-master nil ;; ask for master file on a directory
				TeX-engine 'xetex
				TeX-PDF-mode t
				))

;; Terraform
(use-package terraform-mode)

;; Jenkins
(use-package groovy-mode)

;; OPA
(use-package rego-mode
  :custom
  (rego-format-at-save nil))

;; LSP follow https://www.mattduck.com/lsp-python-getting-started.html
;; Install server https://www.mattduck.com/lsp-python-getting-started.html
;; pip install python-lsp-server

(if (version<= "27.1" emacs-version)
    (use-package lsp-mode
      ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
      ;; M-x lsp-workspace-folders-add stores session under ~/.emacs.d/lsp-session-v1
      :commands (lsp lsp-deferred)
      :custom
      (lsp-keymap-prefix "C-c l")
      (lsp-enable-links nil)
      (lsp-completion-provider :none)
      (lsp-enable-snippet nil)
      :hook (
             ;;(python-mode . lsp)
             (elisp-mode . lsp)
             (lsp-mode . lsp-enable-which-key-integration)
             (lsp-completion-mode-hook
              . (lambda ()
                  (setq-local completion-category-defaults nil))))
      :config
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                        :major-modes '(python-mode)
                        :remote? t
                        :server-id 'pyls-remote))))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-doc-delay 2)
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :bind (
;;          :map lsp-ui-mode-map
;;               ("C-c i" . lsp-ui-imenu)))

;; project navigation
;; (use-package treemacs
;;   :defer t
;;   :custom
;;   (treemacs-no-png-images t)
;;   (treemacs-width 24)
;;   :bind ("C-c t" . treemacs))

;; (use-package lsp-haskell
;;   :config (setq lsp-haskell-process-path-hie "hie-wrapper"))


(use-package nix-mode
  :mode "\\.nix\\'")

;; helper functions
(load-file "~/.emacs.d/functions.el")
