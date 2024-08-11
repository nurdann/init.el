;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;
;; lisp basic
;;;;;;;;;;;;;;;;;;;;

;; Cons cell means a Lisp object has a non-nil item
;; nil is both `(atom nil)` and `(lisp nil)' because `(eq '() nil)'

;; car and cidr are equivalent to head and tail from Haskell
;; (car '(a b c)) => a
;; (cdr '(a b c)) => (b c)

(defun u/append-two-lists (x y)
  (cond ((null x) y)
        (t (cons (car x) (my/append (cdr x) y)))))

(defun u/length-of-list (list)
  (cond ((null list) 0)
        (t (+ 1 (u/length-of-list (cdr list))))))

(defun u/prepend-to-list (x xs)
  (cond ((null xs) (cons x '()))
        (t  (cons x xs))))

(defun u/append-to-list (x xs)
  (cond ((null xs) (cons x '()))
        (t (cons
            (car xs)
            (u/append-to-list x (cdr xs))))
         ))

(defun u/insert-into-list (xs x i)
  ;; https://stackoverflow.com/a/65697090/1374078
  (let* ((padded-list (cons nil xs))
         (insert-cons (nthcdr i padded-list)))
    (setcdr insert-cons (cons x (cdr insert-cons)))
    (cdr padded-list)))

(defun u/replace-in-list (xs x i)
  (setcar (nthcdr i xs) x)
  xs)

(defun u/get-n-items-from-list (xs n)
  (cond ((<= n 0) '())
        (t (cons (car xs) (u/get-n-items-from-list (cdr xs) (- n 1))))))

(defun u/last-item (xs)
  "Get latest non-nil item in string, or nil otherwise"
  (let ((last-item nil))
    (dolist (x (append xs nil))
      (if x (setq last-item x)))
    last-item))

;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;

(defun previous-other-window ()
  (interactive)
  (select-window (previous-window)))

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

(defun u/revert-current-buffer-or-visible-windows ()
	"Revert current buffer, or revert all visible buffers if universal argument `C-u' provided"
  	(interactive)
	 (if (equal current-prefix-arg '(4))
		 (u/revert-visible-windows)
	   (u/revert-current-or-shell-buffer)))

(defun u/revert-visible-windows ()
  "Revert visible unmodified windows"
  (interactive)
  (let ((visible-buffers '()))
	(dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
		;;(message "file %s buffer %s" filename buf)
		(if (and filename (file-readable-p filename)
				 ;; (buffer-modified-p buf) ;; checks only internal changes
                 (not (verify-visited-file-modtime buf))
				 (get-buffer-window buf 'visible)
				 )
			(progn
			  (get-buffer-window buf)
			  (with-current-buffer buf
				(revert-buffer :ignore-auto :preserve-modes))
	  		  (add-to-list 'visible-buffers buf)
			  ;;(message "Updated buffer %s" buf)
              )
	)))
	(message "Reverted buffers: %s" visible-buffers)))

(defun copy-last-message ()
  (interactive)
  "Copy last message from *Message* buffer"
  (if (current-message)
      (kill-new (current-message))
    (kill-new (last-message))))

(defun last-message (&optional num)
  ;; https://unix.stackexchange.com/a/154154/356972
  (or num (setq num 1))
  (if (= num 0)
      (current-message)
    (save-excursion
      (set-buffer "*Messages*")
      (save-excursion
    (forward-line (- 1 num))
    (backward-char)
    (let ((end (point)))
      (forward-line 0)
      (buffer-substring-no-properties (point) end))))))

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

(defun get-previous-line ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (thing-at-point 'line t)))

(defun todo-item-p (line)
  (string-match "^[[:space:]]*\\(\\*\\|-\\) \\[.\\]" line))

(defun current-line-todo-item-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[[:space:]]*\\(\\*\\|-\\) \\[.\\]")))

(defun org-insert-list-item-on-enter ()
  "Insert a new list item on Enter"
  (interactive)
  (cond
   ((current-line-todo-item-p) (org-insert-item 1))
   ((org-in-item-p) (org-insert-item))
   (t (org-return))))

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
  ;; if shell is completely unresponsive to input, run `(kill-process)'
  ;; you can `M-x list-processes' as well
  ;; source: https://stackoverflow.com/a/10628109/1374078
  (if (process-live-p (get-buffer-process (current-buffer)))
      (kill-process (current-buffer)))

  (sleep-for 0.25)
  (shell (current-buffer))
  )

;;;;;;;;;;;;;;;;;;;;
;; revert shell or file
(defun u/revert-current-or-shell-buffer ()
  (interactive)
  "Revert shell buffer, if not revert regular file"
  (cond
      ((buffer-file-name) (revert-buffer :ignore-auto :noconfirm :preserve-modes))
      ((active-minibuffer-window) (message "Minibuffer active, ignoring command..."))
      (t (reinit-current-shell-buffer)))
  )

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

(defun my/tramp-shell (tramp-string &optional buffername)
  "Open shell by opening dired on `remote-string'. If `C-u' prefix given, then specify `buffername'"
  (interactive
   (let* ((a (read-string "Tramp: "))
          (b (if (equal current-prefix-arg '(4))
                 (read-string "Buffer name: ")
               a)))
     (list a b)))
  (message "%s - %s" tramp-string buffername)
  (dired tramp-string)
  (new-shell buffername))

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

;;;;;;;;;;;;;;;;;;;;
;; shell-mode helpers

(defun number-of-newlines-in-PS1 ()
  (count-matches-regex "\n" (shell-command-to-string "$SHELL -ic 'echo $PS1'")))

(defun absolute-beginning-of-line ()
  (interactive)
  (while (not (looking-at "^"))
    (backward-char)))

(defun absolute-beginning-of-line-point ()
  (interactive)
  (save-excursion
    (absolute-beginning-of-line)
    (point)))

(defun copy-shell-command-output ()
  (interactive)
  (let* ((start (save-excursion
                  (forward-line (- (number-of-newlines-in-PS1)))
                  (absolute-beginning-of-line-point)))
         (end (save-excursion
                (beginning-of-line)
                (comint-next-prompt 1)))
         )
    (kill-ring-save start end)
    ))

(defun copy-previous-shell-command-output ()
  (interactive)
  (save-excursion
    (comint-previous-prompt 1)
    (copy-shell-command-output)))

(defun copy-buffer-to-clipboard (buffer)
  (with-current-buffer buffer
    (when (> (buffer-size) 0)
      (copy-region-as-kill (point-min) (point-max)))
    ))

(defun copy-process-output (prog &rest args)
  (call-process-region (region-beginning) (region-end) prog nil "*Process Output*" nil "-1"))

(defun u/shell-command-on-region-to-string (cmd)
  "Similar to https://stackoverflow.com/a/3578396/1374078 "
  (interactive
   (list (read-shell-command "Command on region: ")))

  (let* ((start-end (if (use-region-p)
                      (list (region-beginning) (region-end))
                    (list (point-min) (point-max))))
        (start (nth 0 start-end))
        (end (nth 1 start-end)))
    (with-output-to-string
      (shell-command-on-region start end cmd standard-output))))

(defun u/shell-command-to-string (cmd)
  (interactive
   (list (read-shell-command "Command: ")))
  (shell-command-to-string cmd))

(defun u/copy-command-output ()
  (interactive)
  (let* ((out (if (use-region-p)
                 (call-interactively 'u/shell-command-on-region-to-string)
               (call-interactively 'u/shell-command-to-string)))
         (out (string-trim-right out))
         )
    (kill-new out)
    (message (concat "Copied: " (u/truncate-string out 70)))))

(defun u/truncate-string (s limit)
  (if (<= (length s) limit)
      s
    (concat (substring s 0 limit) "...")))


(defun u/html2org-paste ()
  "Convert clipboard contents from HTML to Org and then paste (yank).
  source: https://stackoverflow.com/a/2548429/1374078
  "
  (interactive)
  (let* (
         (link (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t org"))
         )
    ;; (when (string-match "\\[\\[.*?\\]\\]" link)
    ;;   (insert (match-string 0 link)))
    (insert 
     (replace-regexp-in-string "\n\\'" ""
                               link))
    ))
 
;;;;;;;;;;;;;;;;;;;;
;; Revert buffer without prompt

(defun revert-buffer-without-prompt ()
  "Revert buffer without prompting yes/no unless buffer is modified"
  (interactive)
  (message "Reverted current buffer")
  (revert-buffer nil t))

;;;;;;;;;;;;;;;;;;;;
;; Interactive find-file and open shell
(defun open-shell-in-directory (directory &rest rest)
  (interactive "D")
  (let* ((directory (directory-file-name (expand-file-name directory)))
         (default-directory 
          (if (char-equal (u/last-item directory) ?/)
              directory
            (concat directory "/"))))
    (message "Setting directory to %s" default-directory)
    (call-interactively 'new-shell)))

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

(defun end-of-visual-line-p ()
  (= (point)
     (save-excursion
       (end-of-visual-line)
       (point))))

(defun end-of-visual-line-or-end-of-line ()
  (interactive)
  (if (end-of-visual-line-p)
      (end-of-line)
    (end-of-visual-line)))

(defun trace (a)
  (let ((p (funcall a)))
    (print p)
    p))

(defun add-sudo-to-path ()
  (interactive)
  (let ((find-file-str
         (cond
          (buffer-file-name (buffer-file-name))
          (t default-directory)
          )))
    (message "[%s]" find-file-str)
    (find-file (add-sudo-to-path-str find-file-str (file-remote-p default-directory)))))

(defun add-sudo-to-path-str (path is-remote)
  "
If regular path, e.g. `/home/user', then prepend `/sudo::', i.e. `/sudo::/home/user'
If in tramp-mode, e.g. `/ssh:dev:/home/user', then insert `|sudo:', i.e. `/ssh:dev|sudo::/home/user'
Not reliable to omit hosts in emacs (< 27)
"
  (cond
   (is-remote
    (let* ((splits (split-string path ":"))
           (splits-len (length splits))
           (last-hop-idx (- splits-len 2))
           (last-hop (nth last-hop-idx splits))
           (last-hop (concat last-hop "|sudo:")))
      (u/replace-in-list splits last-hop last-hop-idx)
      (mapconcat 'identity splits ":")))
   (t (concat "/sudo::" path))))

;;;;;;;;;;;;;;;;;;;;
;; string helpers

(defun string-match-last-occurence (regex str)
  (let* ((len (length str))
        (prev-occurence (string-match regex str))
        (occurence prev-occurence))
    (while occurence
      (let ((next-occurence (string-match regex str (match-end 0))))
        (setq prev-occurence occurence
              occurence next-occurence)))
    prev-occurence))

(defun count-matches-regex (regex str &optional start count)
  (let ((count (or count 0))
        (start (or start 0)))
    (if (string-match regex str start)
        (count-matches-regex regex str (match-end 0) (+ count 1))
      count)))

;;;;;;;;;;;;;;;;;;;;
;; no preview tramp buffers in consult
(defun consult-buffer-state-no-tramp ()
  "Buffer state function that doesn't preview recentf and remote files"
  (let ((orig-state (consult--buffer-state))
	    (filter (lambda (cand restore)
		          (if (or restore
                          (string-equal "return" cand)			              
                          )
			          cand
			        nil))))
	(lambda (cand restore)
	  (funcall orig-state (funcall filter cand restore) restore))))

;;;;;;;;;;;;;;;;;;;;
;; Buffers
(defun u/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers.
https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/ "
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;;;;;;;;;;;;;;;;;;;;
;; indent
(defun u/newline-and-indent ()
  (interactive)
  (newline)
  (tab-to-tab-stop))

;;;;;;;;;;;;;;;;;;;;
;; Python helpers
(defun u/toggle-python-buffer ()
  "https://emacs.stackexchange.com/a/68446/20237"
  (interactive)
  (cond ((equal (buffer-name) "*Python*") (previous-buffer))
        ((get-buffer "*Python*") (python-shell-switch-to-shell))
        ((derived-mode-p 'python-mode) (run-python))
        (t (message "Not in python-mode."))))

;;;;;;;;;;;;;;;;;;;;
;; Auto activate Python virtualenv

(defun u/vc-virtualenv ()
  (interactive)
  (pyvenv-mode t)
  (call-interactively 'pyvenv-activate)
  ;;(u/set-python-interpreter)
  (message "Activated virtualenv %s" pyvenv-virtual-env)
  (u/pip-install-lsp))


(defun u/get-nth-parent-dir (path &optional nth)
     (let ((dir (directory-file-name path))
           (nth (or nth 1)))
       (dotimes (i nth dir)
         (setq dir (f-parent dir)))))

(defun u/pip-install-lsp ()
  (interactive)
  (if (and pyvenv-virtual-env
           (string= (executable-find "pip") (concat pyvenv-virtual-env "bin/pip"))
           (not (u/cmd-exit-0 (split-string "pip show python-lsp-server"))))
      (shell-command "pip install python-lsp-server | tail -1"))
  )

(defun u/run-python ()
  (interactive)
  ;;(u/kill-python-shell)
  (let ((default-directory (vc-find-root default-directory ".git")))
    (u/vc-virtualenv)
    (pyvenv-restart-python)
    (run-python)
  ))


;; Restart python shell
(defun u/kill-python-shell ()
  "Run python and pop-up its shell.
   Kill process to solve the reload modules problem.
   http://lgmoneda.github.io/2017/02/19/emacs-python-shell-config-eng.html
  "
  (interactive)
  (when (get-buffer-process "*Python*")
     (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
     (kill-process (get-buffer-process "*Python*"))
     (sleep-for 0.25)))


(setq u/python-file-to-run "")
(defun u/send-file-to-python-shell (python-file)
  (interactive
   (list (read-file-name "Run python file: " default-directory  u/python-file-to-run nil nil)))
  (setq u/python-file-to-run python-file)
  (message "Running Python file %s" u/python-file-to-run)
  (pyvenv-restart-python)
  (sleep-for 0.25)
  (python-shell-send-file u/python-file-to-run nil nil nil t)
  )


;;;;;;;;;;;;;;;;;;;;
;; Copying as Rich Text Format (RTF) to clipboard
;; e.g. <b>foo</b> paste as bolded

(defun u/convert-to-rtf (text)
  (interactive)
  (let (
        (rich-text
         (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]" "{\\\\field{\\\\*\\\\fldinst{HYPERLINK \"\\1\"}}{\\\\fldrslt{\\2}}}" text)))
    (concat "{\\rtf1\\ansi\\ansicpg1252" rich-text "}")
    )
  )

(defun u/macos-copy-as-rtf ()
  "source: https://kitchingroup.cheme.cmu.edu/blog/2016/06/16/Copy-formatted-org-mode-text-from-Emacs-to-other-applications/"
  (interactive)
  ;;(org-html-export-as-html nil nil t)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           )
      (with-current-buffer buf
        (shell-command-on-region (point-min) (point-max) "textutil -stdin -format html -convert rtf -stdout | pbcopy" nil nil nil))
      (kill-buffer buf))))


;; Processes

(defun u/cmd-exit-0 (cmd)
  ;; https://stackoverflow.com/a/23299809/1374078
  (eq (apply 'call-process (car cmd) nil nil nil (cdr cmd)) 0))

(defun u/set-python-interpreter ()
  (cond
   ((executable-find "ipython")
    (setq python-shell-interpreter-args "-i --simple-prompt"))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   (t
    (setq python-shell-interpreter "python"))))


;;;;;;;;;;;;;;;;;;;;
;; org mode helper
(defun farynaio/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring.
source: https://emacs.stackexchange.com/a/60555/20237"
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))


(defun u/insert-current-hour ()
  (interactive)
  (insert (format-time-string "%I:%M " (current-time))))


;;;;;;;;;;;;;;;;;;;;
;; File utils

(defun save-buffer-override ()
  "Save the buffer even if buffer diverged."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

;;;;;;;;;;;;;;;;;;;;
;; Minibuffer related

(defun insert-home-dir ()
  (interactive)
  (if (not (eq (char-before) ?/))
      (insert "/~/")
    (insert "~/")))

;;;;;;;;;;;;;;;;;;;;
;; org
(defun u/org-journal-new-entry ()
  (interactive)
  (let ((org-journal-time-format "%R "))
    (org-journal-new-entry '())))

(defun u/org-journal-new-todo () 
  (interactive)
  (let ((org-journal-time-format ""))
    (org-journal-new-entry '())
    (org-todo)
    ))
