;;; magit/forge/issue.el ---                      -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'magit/forge)

;;; Classes

(defclass magit-forge-issue (magit-forge-topic)
  ((closql-table         :initform issue)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform project)
   (closql-foreign-table :initform project)
   (closql-class-prefix  :initform "magit-forge-")
   (id                   :initarg :id)
   (project              :initarg :project)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author) ; also authorAssociation
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (locked-p             :initarg :locked-p) ; also activeLockReason
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees)
   (cards) ; projectsCards
   (edits) ; userContentEdits
   (labels)
   (participants)
   (posts                :closql-class magit-forge-issue-post)
   (reactions)
   (timeline)
   ))

(defclass magit-forge-issue-post (magit-forge-post)
  ((closql-table         :initform issue-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform issue)
   (closql-foreign-table :initform issue)
   (closql-class-prefix  :initform "magit-forge-issue-")
   (id                   :initarg :id)
   (issue                :initarg :issue)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ))

;;; Query

(cl-defmethod magit-forge-get-issue ((prj magit-forge-project) number)
  (closql-get (magit-db)
              (magit-forge--object-id prj number)
              'magit-forge-issue))

(cl-defmethod magit-forge-get-issue ((number integer))
  (when-let (prj (magit-forge-get-project nil))
    (magit-forge-get-issue prj number)))

;;; Commands

;;;###autoload
(defun magit-issue-browse (issue)
  "Visit the url corresponding to ISSUE using a browser."
  (interactive (list (magit-read-issue "Browse issue")))
  (browse-url (magit-forge--format-url issue 'issue-url-format)))

(defalias 'magit-issue-visit 'magit-issue-browse)

;;; Utilities

(defun magit-read-issue (prompt)
  (let* ((prj     (magit-forge-get-project t))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (oref prj issues))
         (default (magit-issue-at-point))
         (choice  (magit-completing-read
                   prompt
                   (mapcar format choices)
                   nil nil nil nil
                   (and default (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (magit-forge-get-issue prj number))))

;;; Sections

(defun magit-issue-at-point ()
  (magit-section-when issue))

(defvar magit-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'magit-issue-browse)
    (define-key map [remap magit-visit-thing]  'magit-issue-visit)
    map))

(defun magit-insert-issues ()
  (when-let ((prj (magit-forge-get-project nil))
             (issues (oref prj issues)))
    (magit-insert-section (issues nil t)
      (magit-insert-heading "Issues:")
      (let ((format (format "%%-%is %%s\n"
                            (1+ (length
                                 (format "%i" (oref (car issues) number)))))))
        (dolist (issue issues)
          (with-slots (number title) issue
            (magit-insert-section (issue issue)
              (insert (format format
                              (propertize (format "#%s" number)
                                          'face 'magit-dimmed)
                              title))))))
      (insert ?\n))))

;;; _
(provide 'magit/forge/issue)
;;; magit/forge/issue.el ends here
