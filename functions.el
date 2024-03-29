;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;

(defun cut-whole-line-at-cursor ()
  "Cut the region, if no region cut current line"
  (interactive)
  (if (use-region-p)
      (kill-region 1 1 t)
    (kill-region (line-beginning-position) (line-end-position))))

(defun copy-whole-line-at-cursor ()
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

(defun sudo-local-save-buffer ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun revert-current-buffer-or-visible-windows ()
	"Revert current buffer, or revert all visible buffers if universal argument `C-u' provided"
  	(interactive)
	 (if (equal current-prefix-arg '(4))
		 (revert-visible-windows)
	   (revert-buffer)))

(defun revert-visible-windows ()
  "Revert visible unmodified windows"
  (interactive)
  (let ((visible-buffers '()))
	(dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
		;;(message "file %s buffer %s" filename buf)
		(if (and filename (file-readable-p filename)
				 ;;(not (buffer-modified-p buf)
				 (get-buffer-window buf 'visible)
				 )
			(progn
			  (get-buffer-window buf)
			  (with-current-buffer buf
				(revert-buffer :ignore-auto  :preserve-modes))
	  		  (add-to-list 'visible-buffers buf)
			  (message "Updated buffer %s" buf))
		  
		;;(message "No visible window to update")
	)))
	(message "Reverted buffers: %s" visible-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow buffer reverts to be undone

(defun preserve-undo-revert-buffer (&optional ignore-auto noconfirm preserve-modes)
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
(defun electric-add-mode-pairs (hook pairs)
  `(add-hook ,hook
	     (lambda ()
	       (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
	       (setq-local electric-text-pairs electric-pair-pairs)
		   (message "%s" electric-pair-pairs))))

;;;;;;;;;;;;;;;;;;;;
;; show current file path

(defun copy-path ()
  "Copy file path of buffer to kill ring"
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;
;; re-initialize stopped shell buffer
(defun reinit-current-shell-buffer ()
  "re-open shell instance"
  (interactive)
  (shell (current-buffer)))

;;;;;;;;;;;;;;;;;;;;
;; revert shell or file
(defun revert-current-or-shell-buffer ()
  (interactive)
  "Revert shell buffer, if not revert regular file"
  (if buffer-file-name
      (revert-buffer)
    (reinit-current-shell-buffer)))

;;;;;;;;;;;;;;;;;;;;
;; open local shell
(defun local-shell ()
  "open shell on localhost; C-u (universal argument) open with counter"
  (interactive)
  ;;(message "%s" current-prefix-arg)
  (let ((local-shell-name "*shell local*"))
  (if (and (equal current-prefix-arg '(4)) (get-buffer local-shell-name))
      (let ((shell-count (count-prefix-match-buffer local-shell-name)))
	(let ((default-directory (get-default-or-current-directory))
	      (new-shell-name (concat local-shell-name (format "<%d>" shell-count))))
	      (shell new-shell-name)))
    (let ((default-directory (get-default-or-current-directory)))
      (shell local-shell-name)))
  (message "Created %s" local-shell-name)))

(defun remote-shell (remote-string buffer)
  (interactive "sRemote: \nBBuffer name:")
  (dired remote-string)
  (new-shell buffer))

(defun new-shell (name)
  ;; https://www.quora.com/What-does-Tikhon-Jelviss-Emacs-setup-look-like
  "Opens a new shell buffer with the given name in 
asterisks (*shell-name*) in the current directory." 
  (interactive "sName: ") 
  (pop-to-buffer (concat "*shell-" name "*")) 
  (unless (eq major-mode 'shell-mode) 
    (shell (current-buffer))))

(defun get-default-or-current-directory ()
  (interactive)
  (if (and buffer-file-name (not (file-remote-p buffer-file-name)))
      (file-name-directory buffer-file-name)
    (if (and default-directory (not (file-remote-p default-directory)))
	default-directory
      "~/")))

(defun count-prefix-match-buffer (prefix)
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (if (string-prefix-p prefix (buffer-name buf))
	  (setq count (+ 1 count))))
    count))

(defun remote-ssh-shell (ssh-server)
  (interactive "sHost: ")
  (remote-shell (concat "/ssh:" ssh-server ":") ssh-server))

;;;;;;;;;;;;;;;;;;;;
;; Revert buffer without prompt

(defun revert-buffer-without-prompt ()
  "Revert buffer without prompting yes/no unless buffer is modified"
  (interactive)
  (message "Reverted current buffer")
  (revert-buffer nil t))

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


(defun my-company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)))

(defun company-ac-setup ()
  "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  (setq company-require-match 'never)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend
                            company-echo-metadata-frontend)))


;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/#:~:text=In%20Emacs%20there%20are%20two,whitespace%20character%20on%20a%20line.
(defun back-to-indentation-or-beginning-of-line (arg)
   "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; In ido-mode, invoke "C-d" to filter only directories
;; source: https://www.emacswiki.org/emacs/InteractivelyDoThings#h5o-7
(defun ido-switch-to-dired (&optional removep)
  (interactive "P")
  (setq ido-cur-list
	    (cl-remove-if-not (lambda (buf-name)
			             (setq buf (get-buffer buf-name))
			             (when (buffer-live-p buf)
			               (with-current-buffer buf
			                 (eq major-mode 'dired-mode))))
		               ido-cur-list)))

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-d") 'ido-switch-to-dired))

(add-hook 'ido-setup-hook #'bind-ido-keys)
