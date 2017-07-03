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

(defvar helm-phalcon--action
  '(("Open File" . find-file)
    ("Open Directory" . helm-phalcon--open-dired)))

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
    (when (file-exists-p (expand-file-name helm-phalcon-basedir))
      (with-temp-buffer
	(let ((ret (call-process-shell-command (concat "find " helm-phalcon-basedir " -type d") nil t)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let ((line (buffer-substring-no-properties
			 (line-beginning-position) (line-end-position))))
	      (push line paths))
	    (forward-line 1))
	  (reverse paths))))))

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
