;;; magit/forge/pullreq.el ---                    -*- lexical-binding: t -*-

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

(defclass magit-forge-pullreq (magit-forge-topic)
  ((closql-table         :initform pullreq)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform project)
   (closql-foreign-table :initform project)
   (closql-class-prefix  :initform "magit-forge-")
   (id                   :initarg :id)
   (project              :initarg :project)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (merged               :initarg :merged) ; mergedAt
   (locked-p             :initarg :locked-p)
   (editable-p           :initarg :editable-p)
   (cross-repo-p         :initarg :cross-repo-p)
   (base-ref             :initarg :base-ref)
   (base-repo            :initarg :base-repo)
   (head-ref             :initarg :head-ref)
   (head-user            :initarg :head-user)
   (head-repo            :initarg :head-repo)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees)
   (cards) ; projectsCards
   (commits)
   (edits) ; userContentEdits
   (labels)
   (participants)
   (posts                :closql-class magit-forge-pullreq-post)
   (reactions)
   (review-requests)
   (reviews)
   (timeline)
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; activeLockReason, additions, authorAssociation, (baseRefName), baseRefOid,
   ;; bodyHTML, bodyText, canBeRebased, changedFiles, closed, createdViaEmail,
   ;; databaseId, deletions, editor, (headRefName), headRefOid, mergeCommit,
   ;; mergeStateStatus, mergeable, merged, mergedBy, permalink,
   ;; potentialMergeCommit,, reactionGroups, resourcePath, revertResourcePath,
   ;; revertUrl, url, viewer{*}
   ))

(defclass magit-forge-pullreq-post (magit-forge-post)
  ((closql-table         :initform pullreq-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform pullreq)
   (closql-foreign-table :initform pullreq)
   (closql-class-prefix  :initform "magit-forge-pullreq-")
   (id                   :initarg :id)
   (pullreq              :initarg :pullreq)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; pullRequest (same as issue),
   ;; repository (use .pullreq.project),
   ;; authorAssociation, bodyHTML, bodyText, createdViaEmail,
   ;; editor, id, reactionGroups, resourcePath, url, viewer{*}
   ))

;;; Query

(cl-defmethod magit-forge-get-pullreq ((prj magit-forge-project) number)
  (closql-get (magit-db)
              (magit-forge--object-id prj number)
              'magit-forge-pullreq))

(cl-defmethod magit-forge-get-pullreq ((number integer))
  (when-let (prj (magit-forge-get-project nil))
    (magit-forge-get-pullreq prj number)))

;;; Utilities

(defun magit-read-pullreq (prompt)
  (let* ((prj     (magit-forge-get-project nil))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (oref prj pullreqs))
         (default (magit-pullreq-at-point))
         (choice  (magit-completing-read prompt
                                         (mapcar format choices)
                                         nil nil nil nil
                                         (and default
                                              (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (magit-forge-get-pullreq prj number))))

;;; Sections

(defun magit-pullreq-at-point ()
  (magit-section-when pullreq))

(defvar magit-pullreq-section-map
  (let ((map (make-sparse-keymap)))
    map))

(defun magit-insert-pullreqs ()
  (when-let ((prj (magit-forge-get-project nil))
             (pullreqs (oref prj pullreqs)))
    (magit-insert-section (pullreqs nil t)
      (magit-insert-heading "Pull requests:")
      (let ((format (format "%%-%is %%s\n"
                            (1+ (length
                                 (format "%i" (oref (car pullreqs) number)))))))
        (dolist (pullreq pullreqs)
          (with-slots (number title) pullreq
            (magit-insert-section (pullreq pullreq)
              (insert (format format
                              (propertize (format "#%s" number)
                                          'face 'magit-dimmed)
                              title))))))
      (insert ?\n))))

;;; _
(provide 'magit/forge/pullreq)
;;; magit/forge/pullreq.el ends here