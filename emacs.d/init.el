;; revert to pre-27.1 default emacsclient daemon Unix socket location
(setq server-socket-dir (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))

;; Do not display Welcome buffer on GUI init
(setq inhibit-startup-screen t)
;; Tell emacs where your personal elisp lib dir is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
;; (setq package-enable-at-startup nil)
;; Check for installed packages on MELPA, install on startup if needed
(load "~/.emacs.d/init-packages")

;; Load sh-mode for Bash and zsh scripts without shebangs
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))

;;----------------------------------------------------------------------------
;; Column and line behavior for all files
;;----------------------------------------------------------------------------

;; Put line numbers on left column
;; (global-linum-mode 1) ;;

;; insert a space to separate linum-mode line number col from contents of buffer
(if window-system
    (progn)
  (setq linum-format "%d ")
  )

;; Set the number to the number of columns to use.
(setq-default fill-column 90)

;; Show line number in the mode line.
(line-number-mode 1)

;; Show column number in the mode line.
(setq column-number-mode t)

;;----------------------------------------------------------------------------
;; Fonts
;;----------------------------------------------------------------------------
;; Skipping font modification for dotfiles_remote.git Emacs setup

;;----------------------------------------------------------------------------
;; Settings for all .txt files
;;----------------------------------------------------------------------------
;; Add Autofill minor mode to major text mode hooks.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; Automatically remove trailing whitespace when file is saved.
(add-hook 'text-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;;----------------------------------------------------------------------------
;; Emacs Lisp Mode for .el files
;;----------------------------------------------------------------------------
;; taken from https://www.emacswiki.org/emacs/EmacsLispMode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    ;; Delete trailing whitespace when saving buffer
	    (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            ;; Keep M-TAB for `completion-at-point'
            (define-key flyspell-mode-map "\M-\t" nil)
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map
              "\C-x\C-e" 'pp-eval-last-sexp)
            ;; Recompile if .elc exists.
            ;; (add-hook (make-local-variable 'after-save-hook)
            ;;           (lambda ()
            ;;             (byte-force-recompile default-directory)))
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
;; shows you, in the echo area, the argument list of the function call you are currently writing
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode) ;; Spellchecking. Requires Ispell

;;----------------------------------------------------------------------------
;; Set NoTabs for C/C++ modes. Use 4 spaces instead of tab for Athena++ source
;;----------------------------------------------------------------------------
(defun athena-cpp-mode ()
  (setq fill-column 90)
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
;; Use "M-x whitespace-mode" to show spaces as dots, tabs as >>
;; Use "M-x untabify" or "M-x tabify" to convert between the two indentations
(add-hook 'c-mode-common-hook 'athena-cpp-mode)
;; Add Autofill mode to mode hooks.
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)

;; Highlight character at "fill-column" position.
(require 'column-marker)
(set-face-background 'column-marker-1 "red")
(add-hook 'c-mode-common-hook
          (lambda () (interactive)
            (column-marker-1 90)))
;; Why didnt this next line properly load the local buffer value of fill-col?
;; Because "hook functions should be designed so that the order in which
;; they are executed does not matter"
;;             (column-marker-1 fill-column)))

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'c-mode-common-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;; Recommended dependency by flycheck-google-c++
(require 'google-c-style)
;; not sure what these do:
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized)))
 '(custom-safe-themes
   (quote
    ("4528fb576178303ee89888e8126449341d463001cb38abe0015541eb798d8a23" "8d3c5e9ba9dcd05020ccebb3cc615e40e7623b267b69314bdb70fe473dd9c7a8" "5673c365c8679addfb44f3d91d6b880c3266766b605c99f2d9b00745202e75f6" "f23a961abba42fc5d75bf94c46b5688c52683c02b3a81313dd0738b4d48afd1d" "429028d30a6cc349351417616112c9d9346282ee951c874988199c4a28f952f5" "2968a2ef2d6053ef935d6d537b32a94c6bec868706dc937e1049473db9e60645" "417a047001847a55f9e0d9692f2bde644a325ab8a1ef18b22baea8309d9164cb" "3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(flycheck-c/c++-googlelint-executable (expand-file-name "~/dotfiles/cpplint.py"))
 '(org-latex-table-scientific-notation "$%s\\times10^{%s}$")
 '(package-selected-packages
   (quote
    (cmake-mode json-mode ido-grid-mode haskell-mode gnuplot gnuplot-mode markdown-mode flycheck google-c-style yaml-mode org magit)))
 '(safe-local-variable-values
   (quote
    ((org-emphasis-alist
      ("*" bold)
      ("/" italic)
      ("_" underline)
      ("=" org-verbatim verbatim)
      ("~" org-code verbatim)
      ("+"
       (:strike-through nil)))))))

; ensure that cpplint.py can be run from Emacs (isn't completely necessary)
;; (executable-find "cpplint.py")

;; Instead, check that:
;; "C-h v flycheck-c/c++-googlelint-executable" is well defined (flycheck doesn't play well with relative paths)
;; "C-c ! C-c c/c++-googlelint" is able to execute the linter in a subshell
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by GCC on dotfiles_remote platforms. Chain this syntax checker with one that is enabled!
     (flycheck-add-next-checker 'c/c++-gcc
                                '(warning . c/c++-googlelint))))


;; unable to execute on PICSciE machines:
;; '(flycheck-c/c++-googlelint-executable "/home/kfelker/.local/lib/python3.6/site-packages/cpplint.py"))

;; uncomment to start flycheck automatically for C/C++ source code
(add-hook 'c-mode-common-hook 'flycheck-mode)

;;----------------------------------------------------------------------------
;; Haskell specific settings
;;----------------------------------------------------------------------------
;; Set the number of columns to use for Haskell source
(add-hook 'haskell-mode-hook
          (lambda () (interactive)
;;          (setq tab-width 4)
            (setq fill-column 79)))

;; Add Autofill mode to mode hooks.
(add-hook 'haskell-mode-hook 'turn-on-auto-fill)
;; Highlight fill-column when running over
(add-hook 'haskell-mode-hook
          (lambda () (interactive)
            (column-marker-1 79)))

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'haskell-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))
;;----------------------------------------------------------------------------
;; Sh-mode specific settings
;;----------------------------------------------------------------------------
;; Automatically remove trailing whitespace when file is saved.
(add-hook 'sh-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;;----------------------------------------------------------------------------
;; Python specific settings
;;----------------------------------------------------------------------------
(load-library "python")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist)
      python-mode-hook
      '(lambda () (progn
                    (set-variable 'py-indent-offset 4)
                    (set-variable 'indent-tabs-mode nil))))
;; Set the number of columns to use for Python source. PEP8 specifies max=79
;; but we are overriding this limit to max=90 for Athena++
(add-hook 'python-mode-hook
          (lambda () (interactive)
;;	    (setq tab-width 4)
	    (setq fill-column 90)))

;; Add Autofill mode to mode hooks.
(add-hook 'python-mode-hook 'turn-on-auto-fill)

;; Add line numbers to the left column
(defun my-python-mode-linum-hook ()
  (linum-mode 1))
(add-hook 'python-mode-hook 'my-python-mode-linum-hook)

;; Highlight character at "fill-column" position.
(require 'column-marker)
(set-face-background 'column-marker-1 "red")
(add-hook 'python-mode-hook
          (lambda () (interactive)
            (column-marker-1 90)))

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;; All Python checkers are invoked indirectly using python -c ...
;; (rather than a direct call to flake8 or pylint); easy to switch Py2 and Py3

;; Run flake8 through the executable pointed to by python-shell-interpreter
;; instead of system Python. Load anaconda and install "pip install --user flake8"
(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
(add-hook 'python-mode-hook 'flycheck-mode)

;;----------------------------------------------------------------------------
;; load emacs 24's package system. Add MELPA repository.
;;----------------------------------------------------------------------------
(when (>= emacs-major-version 24)
  (require 'package) ;; requiring package.el package
  (add-to-list
   'package-archives
   ;; Uncomment for bleeding-edge repos:
   '("melpa" . "http://melpa.milkbox.net/packages/") ;; needed for google-c-style
   ;; Stable repos:
   ;; '("melpa" . "http://stable.melpa.org/packages/")
   t))

;;----------------------------------------------------------------------------
;; yaml-mode - Simple major mode to edit YAML file for emacs
;;----------------------------------------------------------------------------
(require 'yaml-mode) ;; requiring yaml-mode.el package
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;; To ensure that ENTER key is bound to indenting after new line is inserted
 (add-hook 'yaml-mode-hook
      '(lambda ()
	 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'yaml-mode-hook 'flycheck-mode)

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'yaml-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;;----------------------------------------------------------------------------
;; load solarized color theme
;;----------------------------------------------------------------------------
;; for github user "sellout"'s implementation, color-theme-solarized package
;; using color-theme emacs package backend
;;-------------------------------------------
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq solarized-default-background-mode 'dark)

(load-theme 'solarized t)
(defun set-background-mode (frame mode)
  (setq frame-background-mode mode)
  (frame-set-background-mode frame)
  (when (not (display-graphic-p frame))
    (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
  (enable-theme 'solarized))

(defun switch-theme ()
  (interactive)
  (let ((mode  (if (eq (frame-parameter nil 'background-mode) 'dark)
                   'light 'dark)))
    (set-background-mode nil mode)))

(add-hook 'after-make-frame-functions
          (lambda (frame) ;; Use light for GUI, dark for Terminal frame
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
            (set-background-mode frame mode))))

(set-background-mode nil solarized-default-background-mode)

;; global key binding for main magit command
(global-set-key (kbd "C-c t") 'switch-theme)

;;----------------------------------------------------------------------------
;; magit: an interface to the version control system Git
;;----------------------------------------------------------------------------
;; global key binding for main magit command
(global-set-key (kbd "C-x g") 'magit-status)

;;----------------------------------------------------------------------------
;; Custom settings changed from Emacs interface
;;----------------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;----------------------------------------------------------------------------
;; Centralized, flat storage of ~ Emacs backup files. No tree structure
;;----------------------------------------------------------------------------

;;(setq backup-directory-alist `(("." . "~/.saves"))) ; will separate file path with !

;; Increase number of files saved from 2 (1 actual, 1 backup) to 6 (5 backups)
;; and turn on version control
;; (setq delete-old-versions t ; Don't ask to delete excess backup versions.
;;   kept-new-versions 6
;;   kept-old-versions 2
;;   version-control t) ; This turns on numbered backup files

;;----------------------------------------------------------------------------
;; OR, make backup to a designated dir, mirroring the full path as a tree
;;----------------------------------------------------------------------------

;; return a backup file path of a give file path
;; with full directory mirroring from a root dir
;; non-existant dir will be created. Only works
;; single (non-numbered) backup files!
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let (backup-root bpath)
    (setq backup-root "~/.saves")
    (setq bpath (concat backup-root fpath "~"))
    ;; (make-directory (file-name-directory bpath) bpath)
    (make-directory (file-name-directory bpath) (file-name-directory bpath))
    bpath
  )
  )
(setq make-backup-file-name-function 'my-backup-file-name)

;;----------------------------------------------------------------------------
;; By default, Emacs never backs up versioned files controlled by Git, or
;; other VCS. Change this since we do not commit on every save
;;----------------------------------------------------------------------------

(setq vc-make-backup-files t)

;;----------------------------------------------------------------------------
;; By default, Emacs will only backup the buffer's first save per session.
;; Modify the below command https://www.emacswiki.org/emacs/ForceBackups to
;; match my tree-like central backup folder. Unset buffer-backed-up on save
;;----------------------------------------------------------------------------

  ;; (defun force-backup-of-buffer ()
  ;;   ;; Make a special "per session" backup at the first save of each
  ;;   ;; emacs session.
  ;;   (when (not buffer-backed-up)
  ;;     ;; Override the default parameters for per-session backups.
  ;;     (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
  ;;           (kept-new-versions 3))
  ;;       (backup-buffer)))
  ;;   ;; Make a "per save" backup on each save.  The first save results in
  ;;   ;; both a per-session and a per-save backup, to keep the numbering
  ;;   ;; of per-save backups consistent.
  ;;   (let ((buffer-backed-up nil))
  ;;     (backup-buffer)))
;; first save

;;----------------------------------------------------------------------------
;; Don't clobber symlinks; force Emacs to backup with full file copies
;;----------------------------------------------------------------------------
(setq backup-by-copying t)

;;----------------------------------------------------------------------------
;; Org-mode customizations
;;----------------------------------------------------------------------------
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;; Log time when closing TODO items to DONE
(setq org-log-done 'time)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defun org-adjust-region (b e)
  "Re-adjust stuff in region according to the preceeding stuff."
  (interactive "r") ;; current region
  (save-excursion
    (let ((e (set-marker (make-marker) e))
      (_indent (lambda ()
             (insert ?\n)
             (backward-char)
             (org-indent-line)
             (delete-char 1)))
      last-item-pos)
      (goto-char b)
      (beginning-of-line)
      (while (< (point) e)
    (indent-line-to 0)
    (cond
     ((looking-at "[[:space:]]*$")) ;; ignore empty lines
     ((org-at-heading-p)) ;; just leave the zero-indent
     ((org-at-item-p)
      (funcall _indent)
      (let ((struct (org-list-struct))
        (mark-active nil))
        (ignore-errors (org-list-indent-item-generic -1 t struct)))
      (setq last-item-pos (point))
      (when current-prefix-arg
        (fill-paragraph)))
     ((org-at-block-p)
      (funcall _indent)
      (goto-char (plist-get (cadr (org-element-special-block-parser e nil)) :contents-end))
      (org-indent-line))
     (t (funcall _indent)))
    (forward-line))
      (when last-item-pos
    (goto-char last-item-pos)
    (org-list-repair)
    ))))

(org-defkey org-mode-map (kbd "C-+") 'org-adjust-region)

;; New org-mode exporter in version >= 8.0
(require 'ox-md) ;; gui occasionally freezes with "Invalid key combo"
;; Disable complex relative auto-indentation with TAB
(setf org-adapt-indentation nil)
;;----------------------------------------------------------------------------
;; Unfill commands
;;----------------------------------------------------------------------------
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;----------------------------------------------------------------------------
;; Use Magit-managed git-commit-mode automatically when opening COMMIT_EDITMSG
;; (even from outside Magit, e.g. from cmdline "git commit -v" opening $EDITOR
;; which is "emacsclient -t --alternate-editor=" as of 5/11/18). This is a
;; Git-aware mode that colors diffs, wraps commit titles at 50 chars and bodies
;; at 72 chars. Use C-c C-c to commit, C-c C-k to cancel, etc.
;;----------------------------------------------------------------------------
;; See https://github.com/magit/magit/pull/3070 , or
;; https://emacs.stackexchange.com/questions/17689/git-commit-uses-fundamental-mode-after-magit-update
;; (add-to-list 'auto-mode-alist
;;              (cons git-commit-filename-regexp #'git-commit-setup))

;; Can also use either of the following two solutions--- produces the same result!
(require 'git-commit) ;; this automatically enables global-git-commit-mode
;; (global-git-commit-mode)

;; Seems that I should not run "git commit" without the emacs --daemon started!

;; From Jonas Bernoulli, answer for why "git commit -a" produces magit-diff of HEAD^ and working tree
;; instead of HEAD and working tree; Use C-c C-d to swith to the latter
;; https://emacs.stackexchange.com/questions/41408/magit-shows-magit-diff-with-head-instead-of-head-when-invoked-from-console

;; git-commit-fill-column and git-commit-turn-on-auto-fill are deprecated. Using
;; fill-column variable to hard-wrap the Git commit msg body at 72 characters, as
;; recommended by Linus, et al.
(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))

;; spell-check commit msgs. Flyspell is packaged with Emacs; so is ispell.el interface
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell) ;;turn-on-flyspell)

;; Should disable save-place (if enabled by default), since .git/COMMIT_EDITMSG
;; appears as the same file for each message
;; (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))

;;----------------------------------------------------------------------------
;; InteractivelyDoThings (IDO) for navigating buffers and files
;;----------------------------------------------------------------------------
(require 'ido)
(require 'ido-grid-mode)                ;Better ido display
(ido-mode t)
(setq ido-enable-flex-matching t) ;; if no matches are found using the normal prefix or substring matching.
(setq ido-everywhere t) ;; Toggle use of Ido for all buffer/file reading.
;;(setq ido-use-faces t) ;; highlight matching items: faces are always added for ido-grid-mode
(ido-grid-mode t)

;; ido-vertical-mode presumably does this:
;; Display ido results vertically, rather than horizontally
;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)

;;----------------------------------------------------------------------------
;; Vterm emulation
;;----------------------------------------------------------------------------
(use-package vterm
    :ensure t)
