# helm-directory-find-file

Select directory with helm and select the file in this directory with helm.

## Requirements

- Emacs 24.3 or higher
- helm 2.0 or higher

## Sample Configuration

	(define-key global-map (kbd "C-c l") 'helm-directory-find-file)
	(define-key global-map (kbd "C-c C-l") 'helm-directory-find-file)
	(setq helm-directory-find-file-basedir "/home/masa/src/github.com/phalcon-project/")
