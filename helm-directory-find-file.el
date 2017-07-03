;;; helm-directory-find-file.el --- selecting directory before select the file -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Masashı Mıyaura

;; Author: Masashı Mıyaura
;; URL: https://github.com/masasam/emacs-helm-directory-find-file
;; Version: 0.6.4
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

;; You can select directory before select the file with helm interface.
;; Since the directory has important meanings at the framework,
;; I want to complement with helm only the files that is in the meaningful directory.
;; This package provide it.
;; When you select a directory with helm, the file in that directory can be used with helm.

;;; Code:

(require 'helm)
(require 'helm-mode)
(require 'helm-files)

(defgroup helm-directory-find-file nil
  "Selecting directory before select the file."
  :group 'helm)

(defcustom helm-directory-find-file-basedir nil
  "Search this directory."
  :group 'helm-directory-find-file
  :type 'string)

(defcustom helm-directory-find-file-basedir-list nil
  "List of search this directory."
  :group 'helm-directory-find-file
  :type 'string)

(defvar helm-directory-find-file--action
  '(("Open File" . find-file)
    ("Open Directory" . helm-directory-find-file--open-dired)))

(defun helm-directory-find-file--open-dired (file)
  "Open file with dired as FILE."
  (dired (file-name-directory file)))

(defun helm-directory-find-file--list-candidates ()
  "Helm-directory-find-file list candidates."
  (let ((paths))
    (when (file-exists-p (expand-file-name helm-directory-find-file-basedir))
      (with-temp-buffer
	(let ((ret (call-process-shell-command (concat "find " helm-directory-find-file-basedir " -type d") nil t)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let ((line (buffer-substring-no-properties
			 (line-beginning-position) (line-end-position))))
	      (push line paths))
	    (forward-line 1))
	  (reverse paths))))))

(defun helm-directory-find-file--source (repo)
  "Helm-directory-find-file source as REPO."
  (let ((name (file-name-nondirectory (directory-file-name repo))))
    (helm-build-in-buffer-source name
      :init #'helm-directory-find-file--ls-files
      :action helm-directory-find-file--action)))

(defun helm-directory-find-file--ls-files ()
  "Helm-directory-find-file list candidates."
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (apply #'call-process "ls" nil '(t nil) nil))
      (error "Failed: Can't get file list candidates"))))

;;;###autoload
(defun helm-directory-find-file ()
  "Selecting directory before select the file."
  (interactive)
  (let ((repo (helm-comp-read "Directory: "
                              (helm-directory-find-file--list-candidates)
                              :name "Directory"
                              :must-match t)))
    (let ((default-directory (file-name-as-directory repo)))
      (helm :sources (list (helm-directory-find-file--source default-directory))
            :buffer "*helm-directory-find-file-list*"))))

(defun helm-directory-find-file-change-open (path)
  "Setq helm-directory-find-file-basedir with PATH."
  (setq helm-directory-find-file-basedir path))

(defun helm-directory-find-file-basedir-set ()
  "Setq helm-directory-find-file-basedir."
  (let ((resultlist)
	(basedir-list helm-directory-find-file-basedir-list))
    (while basedir-list
      (push (car basedir-list) resultlist)
      (pop basedir-list))
    resultlist))

(defvar helm-directory-find-file-change-list--source
  (helm-build-sync-source "Change helm-directory-find-file base directory"
    :candidates #'helm-directory-find-file-basedir-set
    :volatile t
    :action (helm-make-actions
             "Change directory" #'helm-directory-find-file-change-open)))

;;;###autoload
(defun helm-directory-find-file-change ()
  "Change helm-directory-find-file-basedir with helm interface."
  (interactive)
  (helm :sources '(helm-directory-find-file-change-list--source) :buffer "*helm-directory-find-file-change*"))

(provide 'helm-directory-find-file)

;;; helm-directory-find-file.el ends here
