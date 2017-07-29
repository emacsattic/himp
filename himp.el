(require 'vimish-fold)

(defvar himp-matchers
  '(
    (python-mode
     . ((group
	 ("import\\s-+[^\s-]+" . python-nav-forward-statement)
	 ("from\\s-+[^\s-]+\\s-+import\\s-+[^\s-]+"
	  . python-nav-forward-statement)
	 (himp-python-tryblock-matcher . himp-python-tryblock-matcher))
	(python-info-current-line-comment-p . python-nav-forward-statement)
	(python-info-docstring-p . python-nav-forward-statement))))
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
	  ((and (looking-at-p "$")
		(not (= (point) (point-max))))
	   (forward-line)
	   (beginning-of-line) t)
	  (t nil))))

(defun himp-match-region-advance (matcher)
  (let ((start (point)))
    (cond
     ((stringp matcher)
      (when (looking-at matcher)
	(re-search-forward matcher)))
     ((and (consp matcher)
	   (stringp (car matcher))
	   (functionp (cdr matcher)))
      (when (looking-at (car matcher))
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
  (himp-save-restriction-widen
   (himp-match-region-advance matcher)))

(defun himp-next-region-advance (matchers)
  (catch 'result
    (ignore
     (himp-skip-space)
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
  (let ((matchers (alist-get major-mode himp-matchers)))
    (unless matchers
      (error "No matchers for %s" major-mode))
    (let (result match lastmatch)
      (while (setq match (himp-next-region-advance matchers))
	(if (eq (car match) (car lastmatch))
	    (setcdr (cdr lastmatch) (cddr match))
	  (add-to-list 'result (setq lastmatch match) t)))
      (mapcar 'cdr result))))

(defun himp-python-narrow-to-block ()
  (save-excursion
    (let ((indentation (current-indentation))
	  (start (line-beginning-position))
	  (end (line-end-position)))
      (while (and (> (point) (point-min))
		  (or (>= (current-indentation) indentation)
		      (python-info-current-line-empty-p)
		      (python-info-current-line-comment-p)))
	(setq start (line-beginning-position))
	(forward-line -1)
	(beginning-of-line))
      (goto-char end)
      (while (and (< (point) (point-max))
		  (or (>= (current-indentation) indentation)
		      (python-info-current-line-empty-p)
		      (python-info-current-line-comment-p)))
	(setq end (line-end-position))
	(forward-line)
	(end-of-line))
      (narrow-to-region start end))))

(defun himp-python-tryblock-matcher ()
  (catch 'result
    (when (himp-match-region-advance "try\\s-*:")
      (himp-python-narrow-to-block)
      (himp-skip-matches (alist-get 'python-mode himp-matchers))
      (let* ((matchers (append
			'("pass\\b")
			(alist-get 'python-mode himp-matchers)))
	     (except
	      (when (himp-match-region "except[^:]*:")
		(while (himp-match-region-advance "except[^:]*:")
		  (when (= (himp-skip-matches matchers) 0)
		    (throw 'result nil)))
		t))
	     (finally
	      (when (himp-match-region-advance "finally\\s-*:")
		(if (= (himp-skip-matches matchers) 0)
		    (throw 'result nil)
		  t))))
	(or except finally)))))

(defmacro himp-save-restriction-widen (&rest body)
  "Save excursion and restriction; widen; evaluate BODY."
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defun himp-unhide ()
  "Unhide regions hidden with `himp-hide'."
  (interactive)
  (dolist (region himp--regions)
    (himp-save-restriction-widen
     (goto-char region)
     (ignore-errors
       (vimish-fold-delete))
     (set-marker region nil)))
  (setq himp--regions nil))

(defun himp-hide ()
  "Hide uninteresting regions in current buffer."
  (interactive)
  (himp-unhide)
  (himp-save-restriction-widen
   (goto-char (point-min))
   (dolist (region (himp-find-regions))
     (let ((start (car region))
	   (end (cdr region))
	   (marker (make-marker)))
       (condition-case err
	   (vimish-fold start end)
	 (error (message "Vimish-fold error: '%s' on region %s" err region)))
       (set-marker marker start)
       (set-marker-insertion-type marker t)
       (add-to-list 'himp--regions marker)))))

(define-minor-mode himp-mode
  "Hide imports/uninteresting stuff at beginning of buffer."
  :lighter " himp"
  :keymap himp-keymap
  (if himp-mode
      (himp-hide)
    (himp-unhide)))
