(require 'vimish-fold)

(defgroup himp nil
  "Hide uninteresting parts (imports) at beginning of buffer."
  :group 'programming)

(defcustom himp-show-line-count nil
  "If non-nil, show number of lines in hidden regions."
  :type 'boolean
  :group 'himp)

(defvar himp-matchers
  '(
    (python-mode
     . ((group
	 ("import\\s-+[^\s-]+" . python-nav-forward-sexp-safe)
	 ("from\\s-+[^\s-]+\\s-+import\\s-+[^\s-]+"
	  . python-nav-forward-sexp-safe)
	 (himp-python-tryblock-matcher . himp-python-tryblock-matcher))
	(himp-python-comment-matcher . python-nav-forward-sexp-safe)
	(python-info-docstring-p . python-nav-forward-sexp-safe))))
  "Alist of matchers per major mode.
Each value of the alist cell is a matcher.
A matcher can be:
    1. A regex.
    2. Cons cell (regex . function).  `regex` will match a region
	and `function` will be called to move point right after the region.
    3. Cons cell (matcher . skipper).  `matcher` will be called
	and should return non-nil if point is at a region to hide
	(like `looking-at'), `skipper` will be called to move point
	right after the region.
    4. A list of the form (group (matchers...)).  `group` is literal symbol
	'group.  `matchers` is a list of matchers.  It can be used to group
	 matchers together.")

(defvar himp-keymap (make-keymap)
  "Keymap to use in `himp-mode'.")

(defvar-local himp--regions nil
  "Markers at hidden regions in current buffer.")

(defun himp-skip-space ()
  (while (cond
	  ((looking-at-p "\\s-")
	   (forward-char) t)
	  ((and (eolp) (not (eobp)))
	   (forward-line)
	   (beginning-of-line) t)
	  (t nil))))

(defun himp-match-region-advance (matcher)
  (himp-skip-space)
  (let ((start (point)))
    (cond
     ((stringp matcher)
      (when (looking-at matcher)
	(goto-char (match-end 0))))
     ((and (consp matcher)
	   (stringp (car matcher))
	   (functionp (cdr matcher)))
      (when (looking-at-p (car matcher))
	(save-restriction (funcall (cdr matcher)))))
     ((and (consp matcher)
	   (functionp (car matcher))
	   (functionp (cdr matcher)))
      (when (save-excursion
	      (save-restriction
		(funcall (car matcher))))
	(save-restriction (funcall (cdr matcher)))))
     ((and (listp matcher)
	   (eq 'group (car matcher))
	   (listp (cdr matcher)))
      (when (save-excursion
	      (save-restriction
		(himp-next-region-advance (cdr matcher))))
	(save-restriction
	  (himp-next-region-advance (cdr matcher)))))
     (t (error "Invalid matcher: %s" matcher)))
    (unless (= start (point))
      (cons start (point)))))

(defun himp-match-region (matcher)
  (save-excursion
   (himp-match-region-advance matcher)))

(defun himp-next-region-advance (matchers)
  (catch 'result
    (ignore
     (dolist (matcher matchers)
       (let ((region (himp-match-region-advance matcher)))
	 (when region
	   (throw 'result (cons matcher region))))))))

(defun himp-skip-matches (matchers)
  (let ((count 0))
    (while (himp-next-region-advance matchers)
      (setq count (1+ count)))
    count))

(defun himp-find-regions ()
  (let ((matchers (cdr (assoc major-mode himp-matchers))))
    (unless matchers
      (error "No matchers for %s" major-mode))
    (let (result match lastmatch)
      (while (setq match (himp-next-region-advance matchers))
	(if (eq (car match) (car lastmatch))
	    (setcdr (cdr lastmatch) (cddr match))
	  (add-to-list 'result (setq lastmatch match) t)))
      (mapcar 'cdr result))))

(defun himp--python-narrow-to-tryblock ()
  (save-excursion
    (let ((start (line-beginning-position))
	  (indentation (current-indentation))
	  last-non-comment)
      (catch 'done
	(while (progn
		 (forward-line)
		 (end-of-line)
		 t)
	  (back-to-indentation)
	  (cond
	   ((or
	     (and (python-info-current-line-comment-p)
		  (> (current-indentation) indentation))
	     (> (current-indentation) indentation)
	     (and (= (current-indentation) indentation)
		  (looking-at-p "except\\b\\|finally\\b")))
	    (setq last-non-comment (line-end-position)))
	   ((or
	     (python-info-current-line-empty-p)
	     (and (<= (current-indentation) indentation)
		  (python-info-current-line-comment-p))))
	   (t
	    (forward-line -1)
	    (throw 'done nil)))
	  (when (= (line-end-position) (point-max))
	    (throw 'done nil))))
      (narrow-to-region start (or last-non-comment (point))))))

(defvar himp--python-inside-tryblock nil
  "Non-nil when point is inside tryblock.")

(defun himp-python-tryblock-matcher ()
  (catch 'result
    (when (himp-match-region-advance "try\\s-*:")
      (let ((himp--python-inside-tryblock t))
	(himp--python-narrow-to-tryblock)
	(when (zerop
	       (himp-skip-matches (cdr (assoc 'python-mode himp-matchers))))
	  (throw 'result nil))
	(let* ((matchers (append
			  '("pass\\b")
			  (cdr (assoc 'python-mode himp-matchers))))
	       (except
		(when (himp-match-region "except[^:]*:")
		  (while (himp-match-region-advance "except[^:]*:")
		    (when (zerop (himp-skip-matches matchers))
		      (throw 'result nil)))
		  t))
	       (finally
		(when (himp-match-region-advance "finally\\s-*:")
		  (if (zerop (himp-skip-matches matchers))
		      (throw 'result nil)
		    t))))
	  (or except finally))))))

(defun himp-python-comment-matcher ()
  (when (python-info-current-line-comment-p)
    (forward-line)
    (or
     himp--python-inside-tryblock
     (save-excursion
       (himp-next-region-advance
	(cdr (assoc 'python-mode himp-matchers))))
     (save-excursion
       (and (prog1 (python-info-current-line-empty-p)
	      (forward-line))
	    (python-info-current-line-empty-p))))))

(defun himp--delete-fold (marker)
  "Remove fold at MARKER."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char marker)
      (ignore-errors
	(vimish-fold-delete)))))

(defun himp--make-fold (beg end)
  "Make fold in range BEG END."
  (condition-case err
      (let* ((vimish-fold-show-lines himp-show-line-count)
	     (header-line-length
	      (save-excursion
		(goto-char beg)
		(length (thing-at-point 'line))))
	     (orig-header-width
	      (and (boundp 'vimish-fold-header-width)
		   vimish-fold-header-width))
	     (vimish-fold-header-width
	      (if vimish-fold-show-lines
		  orig-header-width
		header-line-length)))
	(vimish-fold beg end))
    (error
     (message "Vimish-fold error: '%s' on region %s %s" err beg end))))

(defun himp-unhide ()
  "Unhide regions hidden with `himp-hide'."
  (interactive)
  (dolist (region himp--regions)
    (himp--delete-fold region)
    (set-marker region nil))
  (setq himp--regions nil))

(defun himp-hide ()
  "Hide uninteresting regions in current buffer."
  (interactive)
  (himp-unhide)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (dolist (region (himp-find-regions))
	(let ((start (car region))
	      (end (cdr region))
	      (marker (make-marker)))
	  (himp--make-fold start end)
	  (set-marker marker start)
	  (set-marker-insertion-type marker t)
	  (add-to-list 'himp--regions marker))))))

(define-minor-mode himp-mode
  "Hide imports/uninteresting stuff at beginning of buffer."
  :lighter " himp"
  :keymap himp-keymap
  (if himp-mode
      (himp-hide)
    (himp-unhide)))
