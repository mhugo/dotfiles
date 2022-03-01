;;=====================
;;
;;   Global settings
;;
;;=====================

;; No splash screen
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 22)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)


;; subword mode in each buffer: split word on camelcase
(global-subword-mode 1)
;; do not ask before applying these commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(setq column-number-mode t)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)


;; do not add to kill ring on C-backspace
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key [C-backspace] 'backward-delete-word)

;; use ibuffer to list buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; fix for UTF-8 encoding
(define-coding-system-alias 'UTF-8 'utf-8)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;; start in maximized window
(toggle-frame-maximized)

;; server
(server-start)

;; save and restore session
;;(desktop-save-session 1)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)



;; installation does not work => https://github.com/jwiegley/use-package/issues/900
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))



;;======== PACKAGES ========

;; modeline
;; make sure to execute (all-the-icons-install-fonts) once
(require 'package)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))


;; HYDRA
(use-package hydra :ensure t)

;; window movements (hydra)
(defhydra hydra-zoom (global-map "C-x x")
  "windows"
  ("<left>" shrink-window-horizontally "h. shrink")
  ("<right>" enlarge-window-horizontally "h. enlarge")
  ("<down>" shrink-window "v. shrink")
  ("<up>" enlarge-window "v. shrink"))

;; auto complete loading
(use-package auto-complete :ensure t)
;;(require 'auto-complete-config)
;;(ac-config-default)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; ==== Helm =====
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ))

(use-package helm-projectile
  :ensure t
  :after helm)

(use-package helm-ag
  :ensure t
  :after helm)


;; ==== DIRED ====

;; sets Do what I mean mode for dired (midnight commander)
(require 'dired)
(setq dired-dwim-target 't)
;; key binding for wdired mode
(add-hook 'dired-mode-hook
          (lambda()
            (local-set-key (kbd "C-c C-w") (lambda () (interactive) (wdired-change-to-wdired-mode)))))

;; ==== MAGIT ====
(use-package magit :ensure t)
(global-set-key (kbd "C-x C-g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

(use-package git-link :ensure t)

;; ==== MAGIT FORGE ====
(use-package forge
  :ensure t
  :after magit)
(setq auth-sources '("~/.authinfo"))

;; ==== reload file on C-x C-v
(defun reload-file-aux (want_sudo)
  "Reload file in the current buffer."
  "If WANT_SUDO is t, reloads it as root."
  "If WANT_SUDO is nil, reloads it as non-root, even when it is currently loaded as root."
  (let* ((l (count-lines 1 (point)))
        (fn (buffer-file-name))
        (is_sudo (string-prefix-p "/sudo:" fn)))
    (kill-buffer)
    (cond
     ((and want_sudo (not is_sudo))
      (find-file (concat "/sudo::" fn)))
     ((and (not want_sudo) is_sudo)
      (find-file (replace-regexp-in-string "/sudo:[^:]*:" "" fn)))
     ; default
     (t
      (find-file fn)))
    (forward-line l)))
  
(defun reload-file ()
  "Reload file in the current buffer."
  "If it is already loaded as root, reload it as non-root"
  (interactive)
  (reload-file-aux nil)
  )
(global-set-key (kbd "C-x C-v") 'reload-file)

(defun reload-file-sudo ()
  "Reload file in the current buffer but with root perms."
  (interactive)
  (reload-file-aux t)
  )
(global-set-key (kbd "C-x V") 'reload-file-sudo)

;; ==== PROJECTILE ====
(use-package projectile :ensure t)
;; projectile mode prefix key
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(define-key (current-global-map) [remap projectile-find-file] 'helm-projectile-find-file)
(define-key (current-global-map) [remap projectile-ag] 'helm-projectile-ag)

(projectile-mode +1)

(use-package ag :ensure t)

;; ==== PYTEST ====
;; key bindings for pytest
(use-package pytest :ensure t)
(use-package pyenv-mode :ensure t)
(define-prefix-command 'pytest-map)

(use-package realgud :ensure t)
(defun ods-run-test-at-point-in-pdb ()
  "Run a platform test at point in realgud:pdb"
  (interactive)
  (realgud:run-debugger "pdb" 'pdb-query-cmdline
			'pdb-parse-cmd-args
			'realgud:pdb-minibuffer-history
                        (concat "~/src/platform/run_test_locally.sh --pdb -x -s " (pytest-py-testable))
                        nil))

;; ==== FLYCHECK ====

(use-package flycheck
  :ensure t

  :custom
  (flycheck-disabled-checkers '(rust-cargo))
  (flycheck-pylintrc "~/.pylintrc")
  (flycheck-python-flake8-executable "/usr/bin/flake8")
  
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'rust-mode #'flycheck-rust_setup))

;; multiple checkers for python
(use-package flycheck-pycheckers
  :after flycheck
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
    )

  :custom
  (flycheck-pycheckers-checkers '(flake8 mypy3)))


;; ==== ORG-MODE ====
;;(global-set-key (kbd "C-c l") 'org-store-link)
;;(global-set-key (kbd "C-c a") 'org-agenda)

;;(require 'org-mobile)
;;(setq org-mobile-directory "~/org/mobile")
;;(setq org-mobile-files '("~/org/perso.org"))

;; fix some weird org mode cache issue
(setq org-element-use-cache nil)

;; ==== haskell mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;;=====================
;;
;;        C/C++
;;
;;=====================

(setq c-basic-offset 4)
(c-set-offset 'brace-list-open 0)
(c-set-offset 'brace-list-entry 2)
(c-set-offset 'label 2)
(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)


; c++ mode by default for .h
(setq auto-mode-alist(cons '("\\.h$"   . c++-mode)  auto-mode-alist))

(setq auto-mode-alist(cons '("\\.m$"   . octave-mode)  auto-mode-alist))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; ggtags (GNU global support)
            ;;(ggtags-mode 1)
            ;; C-x h will switch back / from .h / .cpp
            (local-set-key  (kbd "C-x h") 'ff-find-other-file)

            ;; autocomplete-c-headers
            ;(require 'auto-complete-c-headers)
            ;(add-to-list 'achead:include-directories '"/usr/include")
            ;(add-to-list 'achead:include-directories '"/usr/include/qt4/QtCore")
            ;(add-to-list 'achead:include-directories '"/usr/include/qt4/QtGui")
            ;(add-to-list 'achead:include-directories '"/usr/include/c++/5")
            ;(add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu/c++/5")
            ;(add-to-list 'achead:include-directories '"/usr/include/c++/5/backward")
            ;(add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/5/include")
            ;(add-to-list 'achead:include-directories '"/usr/local/include")
            ;(add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed")
            ;(add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu")
            ;(add-to-list 'ac-sources 'ac-source-c-headers)

            ;; turn on semantic mode
            ;(semantic-mode 1)
            ;(global-ede-mode 1)
            ;(global-semantic-idle-scheduler-mode 1)
            ;; add semantic to autocomplete
            ;(add-to-list 'ac-sources 'ac-source-semantic)

            ;; custom project configs
            ;;(my-projects-config)
))

;;
;; dev docs
;;

(use-package devdocs :ensure t)

(global-set-key (kbd "C-h d") 'devdocs-lookup)

(setq python-mode-hook ())
(add-hook 'python-mode-hook
	  (lambda () (setq-local devdocs-current-docs '("python~3.7"))))

;;=====================
;;
;;        Python
;;
;;=====================

(use-package py-isort :ensure t
  :custom
  ;; vertical hanging for multi line imports
  (py-isort-options '("-m" "3")))

(add-hook 'python-mode-hook
      (lambda ()
        ;; tab size for python
        (setq tab-width 4)
        (setq python-indent-offset 4)
        (pyenv-mode)
        ;; Add the following in .dir-locals.el
        ;; (python-mode . ((eval . (pyenv-mode-set "platform_py2"))))
        (local-set-key (kbd "C-c C-t") 'pytest-map)
        (define-key pytest-map (kbd "t") 'pytest-one)
        (define-key pytest-map (kbd "g") 'ods-run-test-at-point-in-pdb)
        (define-key pytest-map (kbd "m") 'pytest-module)
        ;; C-c r calls black on region
        (local-set-key (kbd "C-c r") 'python-black-on-region-or-buffer)
        ;; black before save
        (python-black-on-save-mode)
        ))

;; FIXME
(load-library "realgud")

;; black formatter
;; requires 'black-macchiato' installed
(use-package python-black
  :demand t
  :after python
  :custom
  (python-black-macchiato-command
   "/usr/local/bin/black-macchiato"))

(defun python-black-on-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (python-black-region (region-beginning) (region-end))
    (python-black)))


;;=====================
;;
;;       YAML
;;
;;=====================
(use-package yaml-mode
  :ensure t)

;;=====================
;;
;;       QML
;;
;;=====================
(use-package qml-mode
  :ensure t)


;;=====================
;;
;;       (E)Lisp
;;
;;=====================


;; C-c C-c to evaluate the buffer in lisp mode
(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-buffer)


;;=====================
;;
;;       LSP/DAP
;;
;;=====================


(use-package lsp-mode :ensure t
  :init
  ;; disable lsp in flycheck
  (setq lsp-diagnostic-package :none)
)
(add-hook 'python-mode-hook #'lsp)

(setq lsp-keymap-prefix "C-c l") ;; for documentation only ?
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

(define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
(define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

(use-package lsp-java :ensure t)
(add-hook 'java-mode-hook #'lsp)

;;(use-package dap-python :ensure t)

;(dap-register-debug-template
;  "platform"
;  (list :type "python"
;        :args "runserver 8000"
;        :env '(("DEBUG" . "1"))
;        :program (expand-file-name "~/src/platform/ods/core/manage.py")
;        :request "launch"
;        :name "platform"
;        ))

(use-package dimmer :ensure t)

;; restclient (postman-like)
(use-package restclient
  :ensure t
  :bind (:map restclient-mode-map
              ("C-c c" . restclient-http-send-current-stay-in-window)
              ("C-c r" . json-reformat-at-point)))

;; json-reformat (better json-pretty-print)
(use-package json-reformat
  :ensure t
  )
;;(fset 'json-pretty-print 'json-reformat-region)
(fset 'json-pretty-print 'json-pretty-print-buffer)

;; alternative to json-pretty-print and json-reformat-region
;; it relies on Python, since the two firsts have issues with
;; not-so-standard JSON (including unicode chars for example)
;; FIXME
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

(defun json-reformat-at-point ()
  (interactive)
  (let ((at-method (re-search-backward restclient-method-url-regexp)))
    (when at-method
      (let*
          ;; just after headers
          ((after-headers (save-excursion
                              (goto-char (re-search-forward restclient-empty-line-regexp nil t))
                              (forward-line 1)
                              (point)))
           ;; next restclient body
           (next-comment (save-excursion
                           (goto-char after-headers)
                           ;; either the next comment or the end of the buffer
                           (let ((next-c (re-search-forward restclient-comment-start-regexp nil t)))
                             (if next-c
                                 (progn
                                   (goto-char next-c)
                                   (beginning-of-line)
                                   (point)
                                   )
                               (point-max))))))

        (goto-char next-comment)
        (newline)
        (newline)

        (json-reformat-region after-headers next-comment)

        )))
  )

;; shell
(use-package vterm
  :ensure t)
(global-set-key (kbd "C-c e") 'vterm)

;; ===== FONT =====

(set-face-attribute 'default nil
                    :family "Hack" :height 150 :weight 'normal)

;;=======================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide '.emacs)
