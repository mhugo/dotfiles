;; circumflex accents can now be typed !!
(load-library "iso-transl")

;; sudo
(defun sudo-shell-command (command)
  (interactive "sCommand:")
  (shell-command (concat "echo " (read-passwd "Password: ") " | sudo -S " command))
)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(setq c-basic-offset 4)
(setq column-number-mode t)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(c-set-offset 'brace-list-open 0)
(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)

;; do not add to kill ring on C-backspace
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key [C-backspace] 'backward-delete-word)

;; tab size for python
(add-hook 'python-mode-hook
      (lambda ()
        (setq tab-width 4)
        (setq python-indent-offset 4)))

;; fix for UTF-8 encoding
(define-coding-system-alias 'UTF-8 'utf-8)

;; ask for port number when asking for postgresql connection settings
(require 'sql)
(setq sql-postgres-login-params (append sql-postgres-login-params '(port :default 5432)))
;; truncate lines in SQL
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
;; M-x sql-postgres to connect
;; C-c C-b to execute buffer
;; C-c C-r to execute region
    ;; add a newline when sending something to the SQL buffer
    (defun sql-add-newline-first (output)
       "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
      (concat "\n" output))
    
    (defun sqli-add-hooks ()
      "Add hooks to `sql-interactive-mode-hook'."
      (add-hook 'comint-preoutput-filter-functions
                'sql-add-newline-first))
    
    (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
    
;; define code styles

(c-add-style "postgresql"
             '("bsd"
               (c-auto-align-backslashes . nil)
               (c-basic-offset . 4)
               (c-offsets-alist . ((case-label . +)
                                   (label . -)
                                   (statement-case-open . +)))
               (fill-column . 78)
               (indent-tabs-mode . t)
               (tab-width . 4)))

(defun my-projects-config ()
  (let ((bname (buffer-file-name)))
    (cond
      ((string-match "/SFCGAL/" bname)
       (message "SFCGAL project")
       (setq c-basic-offset 4 )
       (setq ac-clang-flags (append ac-clang-flags '("-I/home/hme/src/SFCGAL/include" "-I/home/hme/libs/include")))
       )

      ((string-match "QGIS/" bname)
       (message "QGIS project")
       (setq c-basic-offset 2 )
       )

      ((string-match "qgis" bname)
       (message "QGIS project")
       (setq c-basic-offset 2 )
       )

      ((string-match "pgwork/" bname)
       (message "PostgreSQL project")
       (c-set-style "postgresql")
       )

      ((string-match "postgis" bname)
       (message "PostGIS project")
       (c-set-style "postgresql")
       )

      (t (message "Generic project"))
     )))


; c++ mode by default for .h
(setq auto-mode-alist(cons '("\\.h$"   . c++-mode)  auto-mode-alist))

(setq auto-mode-alist(cons '("\\.m$"   . octave-mode)  auto-mode-alist))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


(put 'upcase-region 'disabled nil)

;; compilation windows always at the bottom
;(defun my-compilation-hook ()
;  (when (not (get-buffer-window "*compilation*"))
;    (save-selected-window
;      (save-excursion
;        (message "ok")
;        (delete-other-windows)
;        (let* ((w (split-window-vertically))
;               (h (window-height w)))
;          (select-window w)
;          (switch-to-buffer "*compilation*")
;          (shrink-window (- h 20)))))))
;(add-hook 'compilation-mode-hook 'my-compilation-hook)

;; IDO
(require 'ido)
(ido-mode t)

;; subword : Process CamelCase
(subword-mode t)


;; add package repositories
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; does not work
(defun member-to-mcamelcase ()
 (interactive)
 (call-interactively 'query-replace-regexp '("\\([a-zA-Z_]+\\)_" "m\\,(string-inflection-camelcase-function \\1)"))
)

;; does not work
(defun arg-to-camelcase ()
 (interactive)
;; query-replace-regexp \([a-z]+\(_[a-z]+\)+\)( \,(string-inflection-lower-camelcase-function \1)(
)

;; does not work
(defun m-camelcase-word-at-point ()
  (interactive)
  (save-excursion
    (let (s bounds pos1 pos2 pos)
      (setq s (thing-at-point 'word))
      (setq bounds (bounds-of-thing-at-point 'word))
      (setq pos1 (car bounds))
      (setq pos2 (cdr bounds))
      (setq pos 0)
      (while (string-match "\\([a-zA-Z]+\\)_" s pos)
        (setq pos (match-end 0))
        (setq s (replace-match (capitalize (match-string 1 s)) nil nil s 1)))
      (delete-region pos1 pos2)
      (goto-char pos1)
      (insert (concat "m" s))
      )
    )
  )

;; resizing windows
(global-set-key (kbd "C-x <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <down>") 'shrink-window)
(global-set-key (kbd "C-x <up>") 'enlarge-window)

;; auto complete loading
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; ggtags (GNU global support)
            (ggtags-mode 1)
            ;; C-x h will switch back / from .h / .cpp
            (local-set-key  (kbd "C-x h") 'ff-find-other-file)

            ;; autocomplete-c-headers
            (require 'auto-complete-c-headers)
            (add-to-list 'achead:include-directories '"/usr/include")
            (add-to-list 'achead:include-directories '"/usr/include/qt4/QtCore")
            (add-to-list 'achead:include-directories '"/usr/include/qt4/QtGui")
            (add-to-list 'achead:include-directories '"/usr/include/c++/5")
            (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu/c++/5")
            (add-to-list 'achead:include-directories '"/usr/include/c++/5/backward")
            (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/5/include")
            (add-to-list 'achead:include-directories '"/usr/local/include")
            (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed")
            (add-to-list 'achead:include-directories '"/usr/include/x86_64-linux-gnu")
            (add-to-list 'ac-sources 'ac-source-c-headers)

            ;; turn on semantic mode
            ;(semantic-mode 1)
            ;(global-ede-mode 1)
            ;(global-semantic-idle-scheduler-mode 1)
            ;; add semantic to autocomplete
            ;(add-to-list 'ac-sources 'ac-source-semantic)

            ;; custom project configs
            (my-projects-config)
))

;; start in maximized window
(toggle-frame-maximized)
(put 'downcase-region 'disabled nil)

;; helper function to help replace a sexp by its eval in a kbd macro
(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

;; C-c C-c to evaluate the buffer in lisp mode
(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-buffer)

(require 'multiple-cursors)

;; sets Do what I mean mode for dired (midnight commander)
(eval-when-compile (require 'dired))
(setq dired-dwim-target 't)
;; key binding for wdired mode
(add-hook 'dired-mode-hook
          (lambda()
            (local-set-key (kbd "C-c C-w") (lambda () (interactive) (wdired-change-to-wdired-mode)))))


(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'flycheck-pyflakes)

;; magit
(global-set-key (kbd "C-x C-g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; reload file on C-x C-v
(defun reload-file ()
  (interactive)
  (let ((l (count-lines 1 (point)))
        (fn (buffer-file-name)))
    (kill-buffer)
    (find-file fn)
    (forward-line l)
    )
  )
(global-set-key (kbd "C-x C-v") 'reload-file)

;; default font
(add-to-list 'default-frame-alist '(font . "-simp-Hack-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1"))

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(require 'org-mobile)
(setq org-mobile-directory "~/org/mobile")
(setq org-mobile-files '("~/org/perso.org"))

;; fix some weird org mode cache issue
(setq org-element-use-cache nil)

;; haskell mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; server
(server-start)

;; launch terminal
(defun run-gnome-terminal-here ()
  (interactive "@")
  (shell-command (concat "gnome-terminal --working-directory="
            (file-name-directory (or load-file-name buffer-file-name (dired-current-directory))) 
              " > /dev/null 2>&1 & disown") nil nil))
(global-set-key (kbd "C-c C-e") 'run-gnome-terminal-here)

;; unbind it from the C mode
(add-hook 'c-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-e"))))



;; zeal-at-point
(global-set-key (kbd "C-h z") 'zeal-at-point)

(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(dimmer-fraction 0.3)
 '(dimmer-mode t nil (dimmer))
 '(flycheck-disabled-checkers (quote (python-flake8 python-pylint)))
 '(flycheck-pylintrc "~/.pylintrc")
 '(org-agenda-files (quote ("~/org/TODO.org" "~/org/perso.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")
     ("\\.ods\\'" . "libreoffice %s")
     ("\\.odt\\'" . "libreoffice %s")
     ("\\.odp\\'" . "libreoffice %s"))))
 '(package-selected-packages
   (quote
    (paredit flycheck-pyflakes helm-dash zeal-at-point rust-mode dimmer epc edbi htmlize csv-mode yasnippet string-inflection sql-indent solarized-theme pylint pacmacs osc org-present multiple-cursors markdown-mode magit lua-mode lodgeit key-chord highlight haskell-mode ggtags format-sql flycheck epresent elmacro bison-mode auto-complete-c-headers ac-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

    
