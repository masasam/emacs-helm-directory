# helm-phalcon

phalcon complete with helm interface

## Requirements

- Emacs 24.3 or higher
- helm 2.0 or higher

## Sample Configuration

	(define-key global-map (kbd "C-c l") 'helm-phalcon)
	(setq helm-phalcon-basedir "/home/masa/src/github.com/phalcon-project/")
	(setq helm-phalcon-controllers "app/modules/frontend/controllers")
	(setq helm-phalcon-admincontrollers "app/modules/frontend/controllers/Admin")
	(setq helm-phalcon-services "app/models/services/Service")
	(setq helm-phalcon-repositories "app/models/repositories/Repository")
	(setq helm-phalcon-entities "app/models/entities")
	(setq helm-phalcon-criterias "app/models/criterias")
	(setq helm-phalcon-messages "app/messages")
	(setq helm-phalcon-forms "app/utils/Forms")
	(setq helm-phalcon-views "app/modules/frontend/views")
	(setq helm-phalcon-config "app/config")
	(setq helm-phalcon-util "app/util")
