;; C-h c <command> to get output of command sequence

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)
(setq package-check-signature  nil)

(eval-when-compile (require 'use-package))

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

(setq ring-bell-function 'ignore)
;(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;(setq display-line-numbers relative)

(setq indent-tabs-mode nil)


;(infer-indentation-style)
(menu-bar-mode 1)
(size-indication-mode 1)
(tool-bar-mode -1)
;;(add-hook 'find-file-hook 'linum-mode)
(add-hook 'find-file-hook 'linum-relative-mode)
(auto-fill-mode -1)

(put 'set-goal-column 'disabled nil) ;; enable C-x C-n; disable C-u C-x C-n

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;(global-unset-key (kbd "C-z"))
;(global-set-key (kbd "C-z C-z") 'my-suspend-frame)


(let ((frame (framep (selected-frame))))
  (or (eq  t  frame)
      (eq 'pc frame)
      (define-key input-decode-map (kbd "C-[") [C-\[])
      (define-key input-decode-map "\C-i" [C-i])
      (define-key input-decode-map "\C-m" [C-m])
      (define-key input-decode-map "\C-j" [C-j])
     ))

;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'display-buffer-alist
             '("^\\*.*\\*$" . (display-buffer-same-window)))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  ;;:bind ("C-x C-p" . keyfreq-show)
  )

;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-k") 'copy-current-line)

(show-paren-mode 1)

(use-package winner
  :ensure t
  :config (winner-mode 1)
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo)))

(use-package electric
  :ensure t
  :config
  (electric-pair-mode 1)
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\}))))

(use-package auto-complete
  :init (auto-complete-mode t)
  :config
  (ac-set-trigger-key "<tab>")
  (ac-config-default)
  (setq ac-delay 0.02
        ))

(use-package keyfreq
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :bind (("C-?" . undo-tree-redo)
         ("C-/" . undo-tree-undo)))

(global-set-key (kbd "C-x s") 'save-buffer) ;; same as C-x C-s


;;;;;;;;;;;;;;;;;;;;
;; Navigating
;;;;;;;;;;;;;;;;;;;;

;(use-package evil
;  :init
;  (evil-mode t))


(windmove-default-keybindings) ;; Shift <arrow-key> to move around windows

(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-;" . ace-jump-word-mode)
         ("M-j" . ace-jump-word-mode)
	 ("S-SPC" . ace-jump-word-mode)
         ))

(use-package ace-window
  :init (ace-window t)
  (setq aw-keys '(?a ?s ?d ?w ?e)) ;; limit characters
  :bind (("M-a" . ace-window)))

;(use-package helm
;  :init
;  (helm-mode t)
;  (helm-autoresize-mode t) ;; grow buffer as needed
;  (setq helm-split-window-in-side-p t ;; split based on current buffer
;       helm-move-to-line-cycle-in-source t ;; cycle options when reaching end/start of buffer
;       helm-autoresize-max-height 50
;                                       ;helm-autoresize-min-height 25
;       )
;  :bind (("M-x" . helm-M-x)
;        ("C-x f" . helm-find-files)
;        ("C-x b" . helm-buffers-list)
;        ("C-x C-f" . helm-recentf)
;        :map helm-find-files-map
;        ("DEL" . helm-find-files-up-one-level)))
;

(use-package ido
  :config (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  :bind
  (;;("M-," . ido-find-file)
   ;;("M-." . ido-switch-buffer)
   ))

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

	    ;; treat initial number as prefix
            (define-key map (kbd "1") (kbd "C-u 1"))
            (define-key map (kbd "2") (kbd "C-u 2"))
            (define-key map (kbd "3") (kbd "C-u 3"))
            (define-key map (kbd "4") (kbd "C-u 4"))
            (define-key map (kbd "5") (kbd "C-u 5"))
            (define-key map (kbd "6") (kbd "C-u 6"))
            (define-key map (kbd "7") (kbd "C-u 7"))
            (define-key map (kbd "8") (kbd "C-u 8"))
            (define-key map (kbd "9") (kbd "C-u 9"))

	    ;; NAVIGATE
	    (setq scroll-preserve-screen-position t)
	    (define-key map (kbd "p") 'scroll-down-line)
	    (define-key map (kbd "n") 'scroll-up-line)
            (define-key map (kbd "h") (kbd "<left>"))
            (define-key map (kbd "j") (kbd "<down>"))
            (define-key map (kbd "k") (kbd "<up>"))
            (define-key map (kbd "l") (kbd "<right>"))
            (define-key map (kbd "e") (kbd "<end>"))
            (define-key map (kbd "a") (kbd "<home>"))
            (define-key map (kbd "f") 'search-next-char)
            (define-key map (kbd "F") 'search-previous-char)
            (define-key map (kbd "u") 'backward-word)
            (define-key map (kbd "o") 'forward-word)

            (define-key map (kbd "s") 'set-goal-column)
            (define-key map (kbd "S") '(lambda () (interactive) (set-goal-column 1)))

            ;; EDIT
            (define-key map (kbd "/") 'undo)
            (define-key map (kbd "?") 'redo)

            ;; KILL
	    ;; C-u C-SPC jump to mark
	    ;; C-x C-x exchange point and mark
	    (define-key map (kbd "SPC") 'set-mark-command)
            (define-key map (kbd "w") 'kill-region)
	    (define-key map (kbd "W") 'kill-ring-save)
            (define-key map (kbd "d") 'delete-backward-char)
            (define-key map (kbd "D") 'delete-forward-char)
            (define-key map (kbd "i") 'yank)
            
            map))

;(global-unset-key (kbd "S-SPC"))
(global-set-key (kbd "S-<return>") 'navi-mode)
(global-set-key (kbd "M-'") 'navi-mode)

;; <menu> mode

(progn
  (define-prefix-command 'menu-key-map)
  (define-key menu-key-map (kbd "1") 'delete-other-windows)
  (define-key menu-key-map (kbd "2") 'split-window-below)
  (define-key menu-key-map (kbd "3") 'split-window-right)
  (define-key menu-key-map (kbd "4") 'delete-window)
  (define-key menu-key-map (kbd "r") '(lambda ()
                                        "Revert buffer without prompting YES"
                                        (interactive)
                                        (revert-buffer t t)))
  (define-key menu-key-map (kbd "f") 'find-file)
  (define-key menu-key-map (kbd "b") 'switch-to-buffer)
  (define-key menu-key-map (kbd "a") 'mark-whole-buffer)
  (define-key menu-key-map (kbd "x") 'kill-region)
  (define-key menu-key-map (kbd "c") 'kill-ring-save)
  (define-key menu-key-map (kbd "v") 'yank)
  (define-key menu-key-map (kbd "s") 'save-buffer)
  (define-key menu-key-map (kbd "S") 'sudo-save-buffer)

  (define-key menu-key-map (kbd "W") 'eval-last-sexp)
  (define-key menu-key-map (kbd "w") 'eval-defun)

  ;; EDITING
  (define-key menu-key-map (kbd "SPC") 'just-one-space)
  (define-key menu-key-map (kbd "k") '(lambda () (interactive) (kill-buffer (current-buffer))))
  ;; Applicable to html, xml
  ;(define-key menu-key-map (kbd "e") 'sgml-close-tag)
  
)

(global-set-key (kbd "<menu>") 'menu-key-map)
(global-set-key (kbd "C-]") 'menu-key-map)


;; <space> mode

(define-minor-mode space-mode
  "Toggle Space Mode"
  :init-value nil
  :lighter " Space"
  :keymap '()
  :group 'space)

;;(define-key space-mode [remap self-insert-command] 'ignore)
;;(define-key space-mode (kbd "t") 'space-mode)

;(global-set-key (kbd "C-'") 'space-mode)

;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;


(defun slick-cut (start end)
  "Cut the region, if no region cut current line"
  (interactive
   (if mark-active
       (progn
         (message "Cut the current line")
         (list (region-beginning) (region-end)))
     (progn
       ;;(list (line-beginning-position) (line-beginning-position 2))
       (message "Cut the region")
       (list (line-beginning-position) (line-end-position)))
     )))

(advice-add 'kill-region :before #'slick-cut)

(defun slick-copy (start end)
  "Copy the region, if no region copy current line"
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied current line")
     ;;(list (line-beginning-position) (line-beginning-position 2))
     (list (line-beginning-position) (line-end-position))
     )))

(advice-add 'kill-ring-save :before #'slick-copy)

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

;;;;;;;;;;;;;;;;;;;;
;; Extentions
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
