;; -*- lexical-binding: t -*-
;;; org-table-hline-cell.el --- Library to extend various modes

;; Copyright (C) 2024

;; Author: Kishor Datar (kishordatar at gmail)

;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'org)
(require 'org-table)

(defconst edit-prompt-delimiter "------- Edit field and finish with C-c C-c -------\n\n")

(defvar org-table-hline-cell-killed-cell nil)

(defun org-table-hline-cell-copy-cell ()
  (interactive)
  (setq org-table-hline-cell-killed-cell (org-table-hline-cell-parse-cell)))

(defun org-table-hline-cell-create-blank-replacement (lines)
  (list :lines
	(cl-loop for l in (plist-get lines :lines)
		 collect (list :str (make-string (length (plist-get l :str)) ? )))))

(defun org-table-hline-cell-create-cell-replacement (orig repl)
  (let* ((maxlen-orig (cl-loop for l in (plist-get orig :lines) maximize (length (plist-get l :str))))
	 (maxlen-new (cl-loop for l in (plist-get repl :lines) maximize (length (plist-get l :str))))
	 (maxlen (max maxlen-orig (or maxlen-new 0)))
	 (npad (- (length (plist-get orig :lines)) (length (plist-get repl :lines)))))
    (if (< npad 0) (error "Replacement too long"))
    (list :lines
	  (cl-loop for l in (append (plist-get repl :lines) (make-list npad (list :str (make-string maxlen ? ))))
		   collect (list :str (plist-get l :str))))))

(defun org-table-hline-cell-delete-cell ()
  (interactive)
  (let* ((current-cell (org-table-hline-cell-parse-cell)))
    (org-table-hline-cell-replace-cell
     current-cell
     (org-table-hline-cell-create-blank-replacement current-cell)))
  (org-table-align))

(defun org-table-hline-cell-cut-cell ()
  (interactive)
  (setq org-table-hline-cell-killed-cell (org-table-hline-cell-parse-cell))
  (org-table-hline-cell-replace-cell
   org-table-hline-cell-killed-cell
   (org-table-hline-cell-create-blank-replacement org-table-hline-cell-killed-cell))
  (org-table-align))

(defun org-table-hline-cell-post-edit ()
  (interactive)
  (goto-char (point-min))
  (search-forward edit-prompt-delimiter)
  (beginning-of-line)
  (let* ((replacement
	  (list :lines
		(cl-loop while (not (eobp))
			 collect
			 (prog1
			     (list :str
				   (buffer-substring
				    (line-beginning-position)
				    (line-end-position)))
			   (forward-line)))))
	 (org-pt pt))
    (with-current-buffer org-buffer
      (let* ((org-table-hline-cell-killed-cell replacement))
	(goto-char org-pt)
	(org-table-hline-cell-paste-cell))))
  (kill-buffer))

(defun org-table-hline-cell-edit-cell ()
  (interactive)
  (let* ((parent-buf (current-buffer))
	 (parent-pt (point))
	 (parsed-cell (org-table-hline-cell-parse-cell))
	 (editor-buf (get-buffer-create "*Org Table Edit HLine Cell*"))
	 )
    (pop-to-buffer-same-window editor-buf)
    (setq-local org-buffer parent-buf)
    (setq-local pt parent-pt)
    (erase-buffer)
    (insert edit-prompt-delimiter)
    (cl-loop for l in (plist-get parsed-cell :lines)
	     do
	     (insert (plist-get l :str))
	     (insert "\n"))
    (local-set-key (kbd "C-c C-c") 'org-table-hline-cell-post-edit)
    (message "Edit and finish with C-c C-c")))

(defun org-table-hline-cell-paste-cell ()
  (interactive)
  (unless org-table-hline-cell-killed-cell (error "No cell to paste."))
  (let* ((no-trailing-whitespaces
	  (list :lines
		(reverse
		 (seq-drop-while
		  (lambda (l)
		    (string-blank-p (plist-get l :str)))
		  (reverse (plist-get
			    org-table-hline-cell-killed-cell
			    :lines))))))
	 (orig-dest (org-table-hline-cell-parse-cell))
	 (hdiff (- (length
		      (plist-get no-trailing-whitespaces :lines))
		     (length
		      (plist-get orig-dest :lines))))
	 (new-dest))

    (when (> hdiff 0)
      (beginning-of-line)
      (let* ((ln (plist-get (nth 0 (last (plist-get orig-dest :lines))) :line))
	     (ncol (plist-get orig-dest :ncol)))
	(org-table-goto-line ln)
	(org-table-goto-column (1+ ncol) t) ; try to preserve the first line under the hline
	(dotimes (v hdiff)
	  (org-meta-return))))

    (org-table-goto-column (plist-get orig-dest :ncol))
    (setq new-dest (org-table-hline-cell-parse-cell))

    (org-table-hline-cell-replace-cell
     new-dest
     (org-table-hline-cell-create-cell-replacement
      new-dest
      no-trailing-whitespaces))
    (org-table-align)))

(defun org-table-hline-move-below-hline-or-beginning-of-table ()
  (if (not (org-at-table-p))
      (error "Not a table"))
  (beginning-of-line)
    (while (and (org-at-table-p)
		(not (org-at-table-hline-p))
		(not (bobp)))
      (forward-line -1))
    (if (not (bobp))
	(forward-line)))

(defun org-table-hline-cell-trim-trailing-blank-rows ()
  (interactive)
  (while (and (org-at-table-p)
		  (not (org-at-table-hline-p))
		  (not (eobp)))
    (forward-line 1))
  (let* ((eot (not (org-at-table-p))))
    (forward-line -1)
    (beginning-of-line)
    (while (looking-at-p "^[[:blank:]|]+$")
      (org-table-goto-column 1)
      (org-shiftmetaup)
      (unless eot (forward-line -1))
      (beginning-of-line))))

(defun org-table-hline-cell-parse-cell ()
  (if (not (org-at-table-p))
      (error "Not a table"))
  (save-excursion
    (let* ((ncol (org-table-current-column))
	   (lines))
      (org-table-hline-move-below-hline-or-beginning-of-table)

      (while (and (org-at-table-p)
		  (not (org-at-table-hline-p))
		  (not (eobp)))
	(org-table-goto-column ncol nil t)
	(push (list
	       :str (org-table-get-field)
	       :line (org-table-current-line))
	      lines)
	(forward-line))
      (list :ncol ncol :lines (reverse lines)))))

(defun org-table-hline-cell-replace-cell (current repl)
  (if (not (= (length (plist-get current :lines)) (length (plist-get repl :lines))))
      (error "Incompatible replacement"))
  (save-excursion
    (let* ((ncol (plist-get current :ncol))
	   (current-rev (reverse (plist-get current :lines)))
	   (repl-rev (reverse (plist-get repl :lines))))
      (cl-loop for p in (seq-mapn 'list current-rev repl-rev)
	       do
	       (let* ((dest (nth 0 p))
		      (src (nth 1 p)))
		 (org-table-goto-line (plist-get dest :line))
		 (org-table-get-field ncol (plist-get src :str)))))))

(provide 'org-table-hline-cell)

;;; org-table-hline-cell.el ends here
