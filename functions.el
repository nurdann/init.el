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

(defun remote-shell (remote-string)
  (interactive "sRemote:")
  (let ((default-directory remote-string))
    (shell remote-string)))

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

