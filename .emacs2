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

;; IDO
(use-package ido :ensure t)
(ido-mode t)

;; auto complete loading
(use-package auto-complete :ensure t)
;;(require 'auto-complete-config)
;;(ac-config-default)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

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
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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
  (flycheck-pycheckers-checkers '(flake8 mypy2)))


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
            (ggtags-mode 1)
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

;;=====================
;;
;;        Python
;;
;;=====================

(use-package py-isort :ensure t
  :custom
  ;; vertical hanging for multi line imports
  (py-isort-options '("-m" "3")))

;; tab size for python
(add-hook 'python-mode-hook
      (lambda ()
        (setq tab-width 4)
        (setq python-indent-offset 4)))



(add-hook 'python-mode-hook
          (lambda ()
            (pyenv-mode)
            ;; Add the following in .dir-locals.el
            ;; (python-mode . ((eval . (pyenv-mode-set "platform_py2"))))
            (local-set-key (kbd "C-c C-t") 'pytest-map)
            (define-key pytest-map (kbd "t") 'pytest-one)
            (define-key pytest-map (kbd "g") 'ods-run-test-at-point-in-pdb)
            (define-key pytest-map (kbd "m") 'pytest-module)
            ))

;; FIXME
(load-library "realgud")

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
  :bind (("C-c c" . restclient-http-send-current-stay-in-window)))

;; ===== FONT =====

(set-face-attribute 'default nil
                    :family "Hack" :height 150 :weight 'normal)

;;=======================

(provide '.emacs)

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(column-number-mode t)
 '(debug-ignored-errors
   '("^Exit the snippet first!$" beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession mark-inactive))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(dimmer-fraction 0.3)
 '(dimmer-mode t nil (dimmer))
 '(elfeed-feeds
   '(("https://lobste.rs/rss" general)
     ("https://news.ycombinator.com/rss" general)
     ("http://nullprogram.com/feed/" emacs)
     ("http://planet.emacsen.org/atom.xml" emacs)))
 '(elpy-rpc-virtualenv-path 'current)
 '(explicit-bash-args '("--noediting" "--login" "-i"))
 '(explicit-shell-file-name "/bin/bash")
 '(git-link-default-branch "develop")
 '(httprepl-curl-args '("-isS" "--insecure"))
 '(network-security-protocol-checks
   '((version medium)
     (compression medium)
     (renegotiation-info-ext medium)
     (verify-cert medium)
     (same-cert low)
     (null-suite medium)
     (export-kx medium)
     (anon-kx medium)
     (md5-sig medium)
     (rc4-cipher medium)
     (dhe-prime-kx medium)
     (sha1-sig medium)
     (ecdsa-cbc-cipher medium)
     (dhe-kx high)
     (rsa-kx high)
     (3des-cipher high)
     (cbc-cipher high)))
 '(org-babel-load-languages '((emacs-lisp . t) (shell . t) (restclient . t) (http . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-duration-units '(("min" . 1) ("h" . 60) ("d" . 450) ("w" . 2250)))
 '(org-return-follows-link t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(doom-modeline flycheck-pycheckers ob-http ob-restclient helm-dash flycheck-rust rust-mode httprepl flycheck lsp-python-ms restclient use-package bind-key lsp-ui lsp-mode jedi elpygen dap-mode ggtags py-isort elfeed ein pyenv-mode go-mode lsp-java yaml-mode ag realgud exec-path-from-shell pytest yasnippet git-timemachine git-link forge projectile magit flycheck-flake8 dimmer auto-complete))
 '(py-isort-options '("-m" "3"))
 '(pytest-global-name "~/src/platform/run_test_locally.sh")
 '(realgud:pdb-command-name "~/src/platform/run_in_pdb.sh")
 '(realgud:trepan3k-command-name "/Users/hugo/Library/Python/3.8/bin/trepan3k")
 '(safe-local-variable-values
   '((git-link-default-branch . "develop")
     (encoding . utf-8)
     (fci-rule-column . 140)
     (c-comment-only-line-offset 0 . 0)
     (eval progn
           (defun my/point-in-defun-declaration-p nil
             (let
                 ((bod
                   (save-excursion
                     (c-beginning-of-defun)
                     (point))))
               (<= bod
                   (point)
                   (save-excursion
                     (goto-char bod)
                     (re-search-forward "{")
                     (point)))))
           (defun my/is-string-concatenation-p nil "Returns true if the previous line is a string concatenation"
                  (save-excursion
                    (let
                        ((start
                          (point)))
                      (forward-line -1)
                      (if
                          (re-search-forward " \\+$" start t)
                          t nil))))
           (defun my/inside-java-lambda-p nil "Returns true if point is the first statement inside of a lambda"
                  (save-excursion
                    (c-beginning-of-statement-1)
                    (let
                        ((start
                          (point)))
                      (forward-line -1)
                      (if
                          (search-forward " -> {" start t)
                          t nil))))
           (defun my/trailing-paren-p nil "Returns true if point is a training paren and semicolon"
                  (save-excursion
                    (end-of-line)
                    (let
                        ((endpoint
                          (point)))
                      (beginning-of-line)
                      (if
                          (re-search-forward "[ ]*);$" endpoint t)
                          t nil))))
           (defun my/prev-line-call-with-no-args-p nil "Return true if the previous line is a function call with no arguments"
                  (save-excursion
                    (let
                        ((start
                          (point)))
                      (forward-line -1)
                      (if
                          (re-search-forward ".($" start t)
                          t nil))))
           (defun my/arglist-cont-nonempty-indentation
               (arg)
             (if
                 (my/inside-java-lambda-p)
                 '+
               (if
                   (my/is-string-concatenation-p)
                   16
                 (unless
                     (my/point-in-defun-declaration-p)
                   '++))))
           (defun my/statement-block-intro
               (arg)
             (if
                 (and
                  (c-at-statement-start-p)
                  (my/inside-java-lambda-p))
                 0 '+))
           (defun my/block-close
               (arg)
             (if
                 (my/inside-java-lambda-p)
                 '- 0))
           (defun my/arglist-close
               (arg)
             (if
                 (my/trailing-paren-p)
                 0 '--))
           (defun my/arglist-intro
               (arg)
             (if
                 (my/prev-line-call-with-no-args-p)
                 '++ 0))
           (c-set-offset 'inline-open 0)
           (c-set-offset 'topmost-intro-cont '+)
           (c-set-offset 'statement-block-intro 'my/statement-block-intro)
           (c-set-offset 'block-close 'my/block-close)
           (c-set-offset 'knr-argdecl-intro '+)
           (c-set-offset 'substatement-open '+)
           (c-set-offset 'substatement-label '+)
           (c-set-offset 'case-label '+)
           (c-set-offset 'label '+)
           (c-set-offset 'statement-case-open '+)
           (c-set-offset 'statement-cont '++)
           (c-set-offset 'arglist-intro 'my/arglist-intro)
           (c-set-offset 'arglist-cont-nonempty
                         '(my/arglist-cont-nonempty-indentation c-lineup-arglist))
           (c-set-offset 'arglist-close 'my/arglist-close)
           (c-set-offset 'inexpr-class 0)
           (c-set-offset 'access-label 0)
           (c-set-offset 'inher-intro '++)
           (c-set-offset 'inher-cont '++)
           (c-set-offset 'brace-list-intro '+)
           (c-set-offset 'func-decl-cont '++))
     (eval pyenv-mode-set "platform-py2")
     (pyvenv-activate . platform-py2)
     (pytest-global-name . "./run_test_locally.sh")))
 '(yas-global-mode t))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;======================= END ========================
