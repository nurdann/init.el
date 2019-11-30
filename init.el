;; C-h c <command> to get output of command sequence

;; TODO
;; Go outside brackets C-M-u C-M-n

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(package-initialize)
;;(package-refresh-contents)
;;(setq package-check-signature  nil)

(progn
  (eval-when-compile (require 'use-package))
  (require 'bind-key) ;; required for :bind
  )


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
      ;;emacs26 display-line-numbers relative
      indent-tabs-mode nil
      select-enable-clipboard t ;; copy/cut kill-ring to clipboard
      tab-width 4
      set-mark-command-repeat-pop t ;; After C-u C-SPC, C-SPC cycles through the mark ring
      mark-ring-max 16 
      window-combination-resize t
      ;;global-subword-mode 1 ;; Iterat through CamelCase words
      
      )

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



(let ((frame (framep (selected-frame))))
  (or (eq  t  frame)
      (eq 'pc frame)
      (define-key input-decode-map (kbd "C-[") [C-\[])
      (define-key input-decode-map "\C-i" [C-i])
      (define-key input-decode-map "\C-m" [C-m])
      (define-key input-decode-map "\C-j" [C-j])
     ))


;; remap default keys

(global-set-key (kbd "<menu>") ctl-x-map)
(define-key ctl-x-map (kbd "f") 'find-file)
(define-key ctl-x-map (kbd "s") 'save-buffer) ;; same as C-x C-s
(define-key ctl-x-map (kbd "w") '(lambda ()
				   (interactive)
				   (kill-buffer (buffer-name))))
(define-key ctl-x-map (kbd "W") 'kill-buffer-and-window)
(define-key ctl-x-map (kbd "x") 'smex)

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

	    ;; NAVIGATE
	    (setq scroll-preserve-screen-position t)
	    (define-key map (kbd "p") '(lambda ()
                                         (interactive)
                                         (scroll-down-line 2)))
	    (define-key map (kbd "n") '(lambda ()
                                         (interactive)
                                         (scroll-up-line 2)))

            (define-key map (kbd "N") 'scroll-up-command)
            (define-key map (kbd "P") 'scroll-down-command)
            
            (define-key map (kbd "h") 'left-char)
            (define-key map (kbd "j") 'next-line)
            (define-key map (kbd "k") 'previous-line)
            (define-key map (kbd "l") 'right-char)


            (define-key map (kbd "H") (kbd "S-<left>"))
            (define-key map (kbd "J") (kbd "S-<down>"))
            (define-key map (kbd "K") (kbd "S-<up>"))
            (define-key map (kbd "L") (kbd "S-<right>"))

            (define-key map (kbd "e") (kbd "<end>"))
            (define-key map (kbd "a") (kbd "<home>"))
            (define-key map (kbd "u") 'backward-word)
            (define-key map (kbd "o") 'forward-word)

            (define-key map (kbd "f") 'search-next-char)
            (define-key map (kbd "F") 'search-previous-char)
	    (define-key map (kbd "[") 'backward-paragraph)
	    (define-key map (kbd "]") 'forward-paragraph)

	    ;; Parenthesis movement
	    ;; C-M-u go up level
	    ;; C-M-n/p go next/previous paren on the same level
	    ;; C-M-e go to the end of defun
	    ;; C-M-a go to the start of defun
	    ;; C-m-f forward sexp
	    
            ;;(define-key map (kbd "s") 'set-goal-column)
            ;;(define-key map (kbd "S") '(lambda () (interactive) (set-goal-column 1)))

            ;; EDIT
            (define-key map (kbd "z") 'undo)
            (define-key map (kbd "Z") 'redo)
	    (define-key map (kbd "r") 'string-rectangle)
	    (define-key map (kbd "t") 'transpose-words)
	    
            ;; KILL
	    ;; C-u C-SPC jump to mark
	    ;; C-x C-x exchange point and mark
	    (define-key map (kbd "SPC") 'set-mark-command)
            (define-key map (kbd "x") 'kill-region)
	    (define-key map (kbd "c") 'kill-ring-save)
            (define-key map (kbd "v") 'yank)
            (define-key map (kbd "D") 'delete-backward-char)
            (define-key map (kbd "d") 'delete-forward-char)

            ;; INSERT
            (define-key map (kbd "i") 'navi-mode)

            (define-key map (kbd "w") '(lambda ()
                                         (interactive)
                                         (navi-mode -1)
                                         (move-end-of-line -1)
                                         (electric-newline-and-maybe-indent)))

            (define-key map (kbd "s") 'swiper-isearch)

            map))



(global-set-key (kbd "<kp-home>") 'navi-mode)
(global-set-key (kbd "S-<return>") 'navi-mode)

;;;;;;;;;;;;;;;;;;;;
;; <menu> mode

(progn
  (define-prefix-command 'menu-key-map)

  (define-key menu-key-map (kbd "r") 'revert-visible-windows)
  (define-key menu-key-map (kbd "R") '(lambda ()
                                        "Revert buffer without prompting YES"
                                        (interactive)
                                        (revert-buffer t t))))

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'display-buffer-alist
             '("^\\*.*\\*$" . (display-buffer-same-window)))

(use-package winner
  :ensure t
  :config (winner-mode 1)
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo)))

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

(alma/add-mode-pairs 'shell-mode-hook '((?\' . ?\') (?\` . ? \`)))
(alma/add-mode-pairs 'markdown-mode-hook '((?\` . ?\`)))

(use-package company
  :config 
  (add-hook 'after-init-hook 'global-company-mode)

  (add-to-list 'company-backends 'company-dabbrev-code)
  ;;(setq company-dabbrev-ignore-case 1)
  
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-capf)
  (company-tng-configure-default)

  :custom
  (completion-auto-help 'lazy)
  (company-begin-commands '(self-insert-command))
  (company-idle-delay  .2)
  (company-minimum-prefix-legth 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t)
  (company-require-match nil)

  :bind (
         :map company-mode-map
        ))


(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  (progn (if (display-graphic-p)
             ;; C-z is bound to suspend-frame which only minimizes Emacs window in GUI
             (progn
               (global-unset-key (kbd "C-z"))
               (global-set-key (kbd "C-z") 'undo-tree-undo)
               (global-set-key (kbd "C-S-z") 'undo-tree-redo)))))

(use-package shell
  ;; (require 'shell) ;; when not using use-package to initialize shell-mode-map
  :bind (:map shell-mode-map
              ("<up>" . 'comint-previous-input)
              ("<down>" . 'comint-next-input)))
 
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
;;  ww
;;      xb xd xg xk xm xs xw
;;  yy
;;      zb zd zf zg zk zm zp zs zw zx

;;(key-chord-define-global k c)

(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(use-package ace-window
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?q ?w ?e ?r ?t ?z ?x ?c ?v)) ;; limit characters
  :chords (("jf" . ace-window)))

(use-package ido
  :config (ido-mode 1)
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-auto-merge-work-directories-length -1
    ido-use-virtual-buffers t))

(use-package dired
  :delight "Dired "
  :custom
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . 'smex)
	 ("M-X" . 'smex-major-mode-commands)))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
	 :map isearch-mode-map
	 ("C-'" . 'avy-resume)))

(use-package avy
  :ensure
  :custom
  (avy-time-out-seconds 0.7)
  :bind (("C-'" . avy-goto-char-timer)
	 ("C-;" . avy-goto-word-1)
	 ("C-:" . avy-goto-line)))


;;;;;;;;;;;;;;;;;;;;
;; Files

(add-to-list
 'directory-abbrev-alist
 '("^/jou" . "/mnt/mdbackup/journal"))

(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;
;; Functions
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

(defun my-suspend-frame ()
  "Suspend only in non-GUI environment"
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical interface")
    (suspend-frame)))

(defun search-next-char (c)
  "Move cursor to the next character matched"
  (interactive "c")
  ;;(if (char-equal ?char-after ?c))
  (search-forward (char-to-string c) nil nil 1))

(defun search-previous-char (c)
  "Move cursor to the previous character matched"
  (interactive "c")
  (search-forward (char-to-string c) nil nil -1))

(defun bind-key-to-map (map keyout)
  (interactive)
  (let ((key (car keyout))
        (out (nth 1 keyout)))
    (define-key map (kbd key) out)
    (message "Bound %s to %s in %s" key out map)))


(defun bind-keys-to-map (map keys)
  (interactive)
  (let ((xs keys)
        (n 0))
    (while(not(null xs))
      (let ((x (car xs)))
        (bind-key-to-map map x)
        (message "Bound %s to %s" (car x) (nth 1 x) x))
      (setq n (+ 1 n)
            xs (cdr xs))
      )
    (message "Bound %d commands to %s" n map)))

(defun sudo-save-buffer ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; https://www.emacswiki.org/emacs/EshellNavigation
(defun bol-maybe-general-my (prompt &optional alt-bol-fcn)
  ""
  (interactive)
  (if (and (string-match (concat "^" (regexp-quote prompt)
                                 " *$")
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (point)))
           (not (bolp)))
      (beginning-of-line)
    (if alt-bol-fcn
        (funcall alt-bol-fcn)
      (beginning-of-line)
      (search-forward-regexp prompt))))

 (add-hook 'eshell-mode-hook '(lambda ()
                               (local-set-key (kbd "C-a")
                                              'eshell-bol-maybe-my)))

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


;; dired reuse buffer
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory"
       (interactive)
       (let* ((orig (current-buffer))
              (filename (dired-get-filename t t))
              (bye-p (file-directory-p filename)))
         ad-do-it
         (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
           (kill-buffer orig))))))

;;;;;;;;;;;;;;;;;;;;
;; EXTENTIONS
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
