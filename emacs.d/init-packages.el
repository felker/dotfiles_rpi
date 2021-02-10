(require 'package)

;; Bleeding-edge MELPA repository needed for google-c-style
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Main org-mode repo:
;; The latest Org package is bundled with the latest Emacs releases, but GNU Emacs is released
;; more infrequently than Org. Shadow the built-in version with the direct-from-source version.
;; https://emacs.stackexchange.com/questions/27597/how-to-update-org-to-latest-version-using-package-repos-git-clone-in-ubuntu
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

;; For manually compiled/installed packages (not currently using any):
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; site-lisp/ is intended for making libraries available to all users on a given system,

;; list the packages you want
(setq package-list
      '(json-mode ido-grid-mode haskell-mode gnuplot gnuplot-mode markdown-mode
                  flycheck google-c-style yaml-mode org magit vterm use-package))

;; Note, package-install-selected-packages variable in init.el is automatically updated
;; by Emacs when installing new packages. THIS ABOVE LIST is the one that should be
;; modified by the user to automatically install desired packages

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
