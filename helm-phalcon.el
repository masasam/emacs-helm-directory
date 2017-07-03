;;; helm-phalcon.el --- phalcon complete with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-helm-phalcon
;; Version: 0.6.3
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; phalcon complete with helm interface

;;; Code:

(require 'helm)
(require 'helm-mode)
(require 'helm-files)

(defgroup helm-phalcon nil
  "Phalcon complete with helm interface."
  :group 'helm)

(defcustom helm-phalcon-basedir nil
  "Directory where phalcon framework is placed."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-controllers nil
  "Phalcon controllers directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-services nil
  "Phalcon services directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-repositories nil
  "Phalcon repositories directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-entities nil
  "Phalcon entities directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-criterias nil
  "Phalcon criterias directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-messages nil
  "Phalcon messages directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-forms nil
  "Phalcon forms directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-views nil
  "Phalcon views directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-config nil
  "Phalcon config directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-util nil
  "Phalcon util directory."
  :group 'helm-phalcon
  :type 'string)

(defcustom helm-phalcon-public nil
  "Phalcon public directory."
  :group 'helm-phalcon
  :type 'string)

(defvar helm-phalcon--action
  '(("Open File" . find-file)
    ("Open Directory" . helm-phalcon--open-dired)))

(defmacro helm-phalcon-recursive-directory (&rest body)
  "Recursive directory list in directory of BODY."
  `(progn
     (with-temp-buffer
       (let ((ret (call-process-shell-command (concat "ls -d " helm-phalcon-basedir ,@body "/*/") nil t)))
	 (unless (zerop ret)
	   (error "Failed ls"))
	 (goto-char (point-min))
	 (while (not (eobp))
	   (let ((line (buffer-substring-no-properties
			(line-beginning-position) (line-end-position))))
	     (push line paths))
	   (forward-line 1))
	 (reverse paths)))))

(defmacro helm-phalcon--line-string ()
  "Obtain part of the character of the buffer without text attributes."
  `(buffer-substring-no-properties
    (line-beginning-position) (line-end-position)))

(defun helm-phalcon--open-dired (file)
  "Open file with dired as FILE."
  (dired (file-name-directory file)))

(defun helm-phalcon--list-candidates ()
  "Helm phalcon list candidates."
  (let ((paths))
    (push (concat helm-phalcon-basedir helm-phalcon-controllers) paths)
    (helm-phalcon-recursive-directory helm-phalcon-controllers)
    (push (concat helm-phalcon-basedir helm-phalcon-services) paths)
    (helm-phalcon-recursive-directory helm-phalcon-services)
    (push (concat helm-phalcon-basedir helm-phalcon-repositories) paths)
    (helm-phalcon-recursive-directory helm-phalcon-repositories)
    (push (concat helm-phalcon-basedir helm-phalcon-entities) paths)
    (helm-phalcon-recursive-directory helm-phalcon-entities)
    (push (concat helm-phalcon-basedir helm-phalcon-criterias) paths)
    (helm-phalcon-recursive-directory helm-phalcon-criterias)
    (push (concat helm-phalcon-basedir helm-phalcon-messages) paths)
    (helm-phalcon-recursive-directory helm-phalcon-messages)
    (push (concat helm-phalcon-basedir helm-phalcon-forms) paths)
    (helm-phalcon-recursive-directory helm-phalcon-forms)
    (push (concat helm-phalcon-basedir helm-phalcon-config) paths)
    (helm-phalcon-recursive-directory helm-phalcon-config)
    (push (concat helm-phalcon-basedir helm-phalcon-util) paths)
    (helm-phalcon-recursive-directory helm-phalcon-util)
    (push (concat helm-phalcon-basedir helm-phalcon-public) paths)
    (helm-phalcon-recursive-directory helm-phalcon-public)
    (push (concat helm-phalcon-basedir helm-phalcon-views) paths)
    (helm-phalcon-recursive-directory helm-phalcon-views)))

(defun helm-phalcon--source (repo)
  "Helm phalcon helm source as REPO."
  (let ((name (file-name-nondirectory (directory-file-name repo))))
    (helm-build-in-buffer-source name
      :init #'helm-phalcon--ls-files
      :action helm-phalcon--action)))

(defun helm-phalcon--ls-files ()
  "Helm phalcon file list candidates."
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (apply #'call-process "ls" nil '(t nil) nil))
      (error "Failed: Can't get file list candidates"))))

;;;###autoload
(defun helm-phalcon ()
  "Phalcon complete with helm interface."
  (interactive)
  (let ((repo (helm-comp-read "phalcon-list: "
                              (helm-phalcon--list-candidates)
                              :name "phalcon list"
                              :must-match t)))
    (let ((default-directory (file-name-as-directory repo)))
      (helm :sources (list (helm-phalcon--source default-directory))
            :buffer "*helm-phalcon-list*"))))

(provide 'helm-phalcon)

;;; helm-phalcon.el ends here
