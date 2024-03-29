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

;;(menu-bar-mode -1)            ; Disable the menu bar

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

;; files with long lines
(global-so-long-mode 1)

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

(use-package projectile
  :ensure t)

;; ==== Expand region ====
(use-package expand-region
  :ensure t
  :bind
  ("C-*" . er/expand-region)
  ("C-ù" . er/contract-region)
  )

;; ==== Ivy/Counsel =====
;; restrict counsel git grep to files with the same extension as the current buffer file
;; see https://emacs.stackexchange.com/questions/37247/specifying-filetype-when-using-counsel-git-grep
(defun counsel-git-grep-current-file-extension ()
  "Like `counsel-git-grep', but limit to current file extension."
  (interactive)
  (pcase-let ((`(_ . ,cmd) (counsel--git-grep-cmd-and-proj nil))
          (ext (file-name-extension (buffer-file-name))))
    (counsel-git-grep nil nil (format "%s -- '*.%s'" cmd (shell-quote-argument ext)))))

(use-package counsel
  :ensure t
  :after projectile
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-c p f" . counsel-git)
         ("C-c p F" . projectile-find-file)
         ("C-c p s s" . counsel-git-grep)
         ("C-c p s S" . counsel-git-grep-current-file-extension)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-s" . counsel-grep-or-swiper)
         ("C-r" . counsel-grep-or-swiper-backward)
         )
  :init
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done))




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

(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; ==== haskell mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; ==== Jenkinsfile mode
(use-package jenkinsfile-mode :ensure t)

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
  (py-isort-options '("--profile" "black")))

(defun shell-command-on-buffer (command)
  (let ((line (line-number-at-pos)))
    ;; replace buffer with output of shell command
    (shell-command-on-region (point-min) (point-max) command nil t)
    ;; restore cursor position
    (goto-line line)
    (recenter-top-bottom)))

(defun python-convert-to-py37 ()
  (interactive)
  (shell-command-on-buffer "~/.pyenv/versions/platform-py37/bin/pyupgrade --py3-plus --py36-plus --py37-plus -"))

(defun python-convert-type-annotations ()
  (interactive)
  (shell-command-on-buffer "cat - >/tmp/com2ann.i && ~/.pyenv/versions/last/bin/com2ann -s /tmp/com2ann.i -o /tmp/com2ann.o && cat /tmp/com2ann.o"))


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

;; documentation

;; add pylookup to your loadpath, ex) ~/.emacs.d/pylookup
(setq pylookup-dir "~/src/pylookup")
(add-to-list 'load-path pylookup-dir)

;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)


;;=====================
;;
;;       YAML
;;
;;=====================
(use-package yaml-mode
  :ensure t)


;;=====================================
;;
;;          rainbow-mode
;;
;; minor mode to set foreground color
;; of an hex color code to its color
;;
;;=====================================
(use-package rainbow-mode
  :ensure t)

;;=====================
;;
;;       QML
;;
;;=====================
(use-package qml-mode
  :ensure t
  :config
  (add-hook 'qml-mode-hook #'rainbow-mode))


;;=====================
;;
;;       (E)Lisp
;;
;;=====================


;; C-c C-c to evaluate the buffer in lisp mode
(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-buffer)

;; highlight corresponding parenthesis
(show-paren-mode 1)


;;=====================
;;
;;       Rust
;;
;;=====================

;; Format buffer on save
(setq rust-format-on-save t)

;;=====================
;;
;;       LSP/DAP
;;
;;=====================


(use-package lsp-mode :ensure t
   :init
   ;; disable lsp in flycheck
   (setq lsp-diagnostic-package :none)
   :config
   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))
;; (add-hook 'python-mode-hook #'lsp)

;; (setq lsp-keymap-prefix "C-c l") ;; for documentation only ?
;; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

;; (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
;; (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

;; (use-package lsp-java :ensure t)
;; (add-hook 'java-mode-hook #'lsp)

(setq lsp-keymap-prefix "C-c l")

(use-package dumb-jump :ensure t)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

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

(use-package jq-mode
  :ensure t)

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

;; elfeed - RSS reader
(use-package elfeed
  :ensure t)

(setq elfeed-feeds
      '("https://simonwillison.net/atom/entries/"
        "https://simonwillison.net/atom/links/"
        "https://nullprogram.com/feed/"
        "https://kx.studio/News/?action=feed"))

;; ===== FONT =====

(set-face-attribute 'default nil
                    :family "Hack" :height 150 :weight 'normal)

(set-face-attribute 'region nil :background "gold" :foreground "gtk_selection_fg_color")

;; ===== Common Lisp =====

(use-package slime
  :ensure t)
(setq inferior-lisp-program "/usr/bin/sbcl")

;; which key

(use-package which-key
  :ensure t)
(which-key-mode)

;; emoji
(use-package emojify
  :ensure t
  :config
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)))


;; undo-tree
;; C-_ : undo
;; M-_ : redo
;; C-x u : tree visualization
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; Prevent undo tree files from polluting your git repo
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;;=======================

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide '.emacs)


;;==========================
;;; restclient-jq.el --- Support for setting restclient vars from jq expressions -*- lexical-binding: t; -*-
;;
;; Public domain.

;; URL: https://github.com/pashky/restclient.el
;; Author: Cameron Dorrat <cdorrat@gmail.com>
;; Maintainer: Cameron Dorrat <cdorrat@gmail.com>
;; Created: 26 Apr 2020
;; Keywords: http jq
;; Package-Requires: ((restclient "20200502.831") (jq-mode "0.4.1") (emacs "24.4"))

;; This file is not part of GNU Emacs.
;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; This is a companion to restclient.el to add support for setting variables from results using jq expressions

;;; Code:
;;
(require 'jq-mode)

;; --- jq support
(defun restclient-jq-result-end-point ()
  (save-excursion
    (goto-char (point-max))
    (or (and (re-search-backward "^[^/].*" nil t)
	     (line-end-position))
	(point-max))))

(defun restclient-jq-get-var (jq-pattern)
  (with-temp-buffer
    (let ((output (current-buffer)))
      (with-current-buffer restclient-same-buffer-response-name
        (call-process-region
         (point-min)
         (restclient-jq-result-end-point)
         shell-file-name
         nil
         output
         nil
         shell-command-switch
         (format "%s %s %s"
                 jq-interactive-command
		 "-r"
                 (shell-quote-argument jq-pattern))))
      (string-trim (buffer-string)))))

(defun restclient-jq-json-var-function (args args-offset)
  (save-match-data
    (and (string-match "\\(:[^: \n]+\\) \\(.*\\)$" args)
         (let ((var-name (match-string 1 args))
               (jq-patt (match-string 2 args)))
           (lambda ()
             (let ((resp-val (restclient-jq-get-var jq-patt)))
               (restclient-remove-var var-name)
               (restclient-set-var var-name resp-val)
               (message "restclient var [%s = \"%s\"] " var-name resp-val)))))))

(defun restclient-jq-interactive-result ()
  (interactive)
  (flush-lines "^//.*") ;; jq doesnt like comments
  (jq-interactively (point-min) (restclient-jq-result-end-point)))


;; run jq pattern over the result buffer
(defun restclient-jq (args args-offset)
  (lexical-let ((cargs args)) ;; lexical-let is needed to create a closure
    (lambda ()
      (shell-command-on-region
       ;; start
       (point-min)
       ;; end
       (restclient-jq-result-end-point)
       ;; command
       (concat "jq " (shell-quote-argument cargs))
       ;; output buffer
       (current-buffer)
       ;; replace
       t))))

;; todo: eval-after-load should be used in configuration, not
;; packages. Replace with a better solution.
(eval-after-load 'restclient
  '(progn
     (restclient-register-result-func
      "jq-set-var" #'restclient-jq-json-var-function
      "Set a restclient variable with the value jq expression,
       takes var & jq expression as args.
       eg. -> jq-set-var :my-token .token")
     (restclient-register-result-func
      "jq" #'restclient-jq
      "Run a jq expression on the output")
     (define-key restclient-response-mode-map  (kbd "C-c C-j") #'restclient-jq-interactive-result)))

(defun restclient-decode-url ()
  (interactive)
  (save-excursion
    (goto-char (restclient-current-min))
    (when (re-search-forward "?" (line-end-position)
                             ; return nil when not found instead of signaling an error
                             t)
      (replace-match "\n- ")
      (beginning-of-line)
      (let ((block-start (point)))
        (while (re-search-forward "&" (line-end-position) t)
          (replace-match "\n- "))
      
        ;; now we are at the end of the "- " parameters block
        (let ((block-end (point)))

          (goto-char block-start)
          (while (looking-at "- ")
            (when (re-search-forward "=\\(.*\\)" (line-end-position) t)
              (message "%s" (match-string 1))
              (replace-match (url-unhex-string (match-string 0))
                                        ; fixedcase
                             nil
                                        ; literal
                             t))
            (forward-line)
            (beginning-of-line))
          
          )
        )
)))


;;; restclient-jq.el ends here

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun urlencode-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun urldecode-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

;;====================
;;    ORG PROTOCOL
;;====================

(require 'org-protocol)

(setq org-capture-templates
        '(
          ("b" "Web Bookmarks" entry
           (file+headline "~/Orgzly/bookmarks.org" "INBOX")
           ;; %a : link with title
           ;; %^g : ask for tags
           "* %a %^g"
           :immediate-finish nil
           )))


;;====================
;;    Emacs shell
;;====================

(use-package bash-completion
  :ensure t)

(autoload 'bash-completion-dynamic-complete
          "bash-completion"
          "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)

(provide 'init)
;;; init.el ends here
