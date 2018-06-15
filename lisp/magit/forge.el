;;; magit/forge.el ---                            -*- lexical-binding: t -*-

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

(require 'magit)
(require 'magit/forge/db)

;;; Options

(defgroup magit-forge nil
  "Options concerning Git forges."
  :group 'magit)

(defcustom magit-forge-alist
  '(("github.com" "api.github.com"    "github" magit-github-project)
    ("gitlab.com" "gitlab.com/api/v4" "gitlab" magit-gitlab-project))
  "List of Git forges.

Each entry has the form (GITHOST APIHOST ID CLASS).

GITHOST is matched against the host part of Git remote urls
  using `magit--forge-url-regexp' to identify the forge.
APIHOST is the api endpoint of the forge's api.
ID is used to identify the forge in the local database.
CLASS is the class to be used for projects from the forge.

GITHOST and APIHOST can be changed, but ID and CLASS are final.
If you change ID, then the identity of every project from that
forge changes.  If you change CLASS, then things start falling
apart.

There can be multiple elements that only differ in GITHOST.
Among those, the canonical element should come first.  Any
elements that have the same APIHOST must also have the same
ID, and vice-versa."
  :package-version '(magit . "2.90.0")
  :group 'magit-forge
  :type '(repeat (list (string :tag "Git host")
                       (string :tag "ID")
                       (string :tag "API endpoint")
                       (symbol :tag "Project class"))))

;;; Classes

(defclass magit-forge-object (closql-object) () :abstract t)

(defclass magit-forge-project (magit-forge-object)
  ((closql-class-prefix       :initform "magit-")
   (closql-class-suffix       :initform "-project")
   (closql-table              :initform project)
   (closql-primary-key        :initform id)
   (issue-url-format          :allocation :class)
   (pullreq-url-format        :allocation :class)
   (create-issue-url-format   :allocation :class)
   (create-pullreq-url-format :allocation :class)
   (id                        :initarg :id)
   (forge                     :initarg :forge)
   (owner                     :initarg :owner)
   (name                      :initarg :name)
   (apihost                   :initarg :apihost)
   (githost                   :initarg :githost)
   (remote                    :initarg :remote)
   (issues                    :closql-class magit-forge-issue)
   (pullreqs                  :closql-class magit-forge-pullreq)))

(defclass magit-forge-topic (magit-forge-object) () :abstract t)

(defclass magit-forge-post (magit-forge-object) () :abstract t)

;;; Core

(defconst magit--forge-url-regexp "\
\\`\\(?:git://\\|git@\\|ssh://git@\\|https://\\)\
\\(.*?\\)[/:]\
\\(\\([^:/]+\\)/\\([^/]+?\\)\\)\
\\(?:\\.git\\)?\\'")

(cl-defmethod magit-forge--object-id ((prj magit-forge-project) number)
  "Return the id of the specified topic."
  (format "%s:%s" (oref prj id) number))

(cl-defmethod magit-forge-get-project ((topic magit-forge-topic))
  "Return the object for the project that TOPIC belongs to."
  (closql-get (magit-db)
              (oref topic project)
              'magit-forge-project))

(cl-defmethod magit-forge-get-project ((demand symbol))
  "Return the project object for the current repository if any.

If DEMAND is nil and the project of the current repository cannot
be determined or the corresponding object does not exist in the
forge database, then return nil.

If DEMAND is non-nil and the project object does not exist in the
forge database yet, then create, insert and return the object.
Doing so involves an API call.  If the required information
cannot be determined, then raise an error."
  (magit--with-refresh-cache
      (list default-directory 'magit-forge-get-project demand)
    (cl-block get-project
      (cl-flet ((return (msg &rest args)
                        (if demand
                            (error "Cannot determine forge project. %s"
                                   (format msg args))
                          (cl-return-from get-project nil))))
        (let ((host    (magit-get "forge.host"))
              (project (magit-get "forge.project"))
              (remote  (or (magit-get "forge.remote")
                           (let ((remotes (magit-list-remotes)))
                             (cond ((and (not (cdr remotes)) (car remotes)))
                                   ((member "origin" remotes) "origin")))))
              owner name spec)
          (cond
           ((and host project)
            (if (string-match "\\`\\([^/]+\\)/\\([^/]\\)\\'" project)
                (progn (setq owner (match-string 1 project))
                       (setq name  (match-string 2 project))
                       (setq spec  (car (cl-member host magit-forge-alist
                                                   :key #'cadr :test #'equal))))
              (return "Invalid forge.project: %s" project)))
           ((or host project)
            (return "Specify both `forge.host' and `forge.project' or neither"))
           (t
            (unless remote
              (if (magit-list-remotes)
                  (return "Cannot decide on remote to use")
                (return "No remote or explicit configuration")))
            (let ((url (magit-git-string "remote" "get-url" remote)))
              (unless url
                (return "No url configured for %s" remote))
              (if (string-match magit--forge-url-regexp url)
                  (progn (setq host  (match-string 1 url))
                         (setq owner (match-string 3 url))
                         (setq name  (match-string 4 url))
                         (setq spec  (assoc host magit-forge-alist)))
                (return "Cannot parse %s's url: %s" remote url)))))
          (unless spec
            (return "%S doesn't match any entry in `magit-forge-alist'" host))
          (pcase-let* ((`(,githost ,apihost ,forge ,class) spec)
                       (db (magit-db))
                       (row (car (emacsql db [:select * :from project
                                              :where (and (= forge $s1)
                                                          (= owner $s2)
                                                          (= name  $s3))]
                                          forge owner name))))
            (cond (row
                   (let ((prj (closql--remake-instance class db row t)))
                     (oset prj apihost apihost)
                     (oset prj githost githost)
                     (oset prj remote  remote)
                     prj))
                  (demand
                   (let ((id (magit-forge--object-id
                              class forge apihost owner name)))
                     (unless id
                       (return "Failed to retrieve project id"))
                     (closql-insert db (funcall class
                                                :id      id
                                                :forge   forge
                                                :owner   owner
                                                :name    name
                                                :apihost apihost
                                                :githost githost
                                                :remote  remote)))))))))))

;;; Utilities

(cl-defmethod magit-forge--format-url ((topic magit-forge-topic) slot)
  (magit-forge--format-url (magit-forge-get-project topic) slot
                           `((?i . ,(oref topic number)))))

(cl-defmethod magit-forge--format-url ((prj magit-forge-project) slot &optional spec)
  (format-spec
   (eieio-oref-default prj slot)
   `((?h . ,(oref prj githost))
     (?o . ,(oref prj owner))
     (?n . ,(oref prj name))
     ,@spec)))

(defconst magit--github-url-regexp "\
\\`\\(?:git://\\|git@\\|ssh://git@\\|https://\\)\
\\(.*?\\)[/:]\
\\(\\([^:/]+\\)/\\([^/]+?\\)\\)\
\\(?:\\.git\\)?\\'")

(defun magit--github-url-p (url)
  (save-match-data
    (and url
         (string-match magit--github-url-regexp url)
         (let ((host (match-string 1 url)))
           ;; Match values like "github.com-as-someone", which are
           ;; translated to just "github.com" according to settings
           ;; in "~/.ssh/config".  Theoretically this could result
           ;; in false-positives, but that's rather unlikely.  #3392
           (and (or (string-match-p (regexp-quote "github.com") host)
                    (string-match-p (regexp-quote (ghub--host)) host))
                host)))))

(defun magit--github-remote-p (remote)
  (or (--when-let (magit-git-string "remote" "get-url" "--push" remote)
        (magit--github-url-p it))
      (--when-let (magit-git-string "remote" "get-url" "--all" remote)
        (magit--github-url-p it))))

(defun magit--github-url-equal (r1 r2)
  (or (equal r1 r2)
      (save-match-data
        (let ((n1 (and (string-match magit--github-url-regexp r1)
                       (match-string 2 r1)))
              (n2 (and (string-match magit--github-url-regexp r2)
                       (match-string 2 r2))))
          (and n1 n2 (equal n1 n2))))))

;;; Libraries

(provide 'magit/forge)

(require 'magit/forge/issue)
(require 'magit/forge/pullreq)

(require 'magit/forge/github)
(require 'magit/forge/gitlab)

;;; Commands

;;;###autoload
(defun magit-forge-pull ()
  "Pull topics from the forge project of the current repository."
  (interactive)
  (if-let (prj (magit-forge-get-project t))
      (progn (magit-forge--pull-issues prj)
             (magit-forge--pull-pullreqs prj)
             (magit-refresh))
    (error "Cannot determine forge project for %s" (magit-toplevel))))

;;;###autoload
(defun magit-forge-reset-database ()
  "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development."
  (interactive)
  (when (and (file-exists-p magit-forge-database-file)
             (yes-or-no-p "Really trash Magit's database file? "))
    (when magit--db-connection
      (emacsql-close magit--db-connection))
    (delete-file magit-forge-database-file t)
    (magit-refresh)))

;;; _
;;; magit/forge.el ends here
