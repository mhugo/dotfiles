;; No splash screen please ... jeez
(setq inhibit-startup-message t)

(setq c-basic-offset 2)
(c-set-offset 'brace-list-open 0)
(c-set-offset 'substatement-open 0)

(add-to-list 'load-path "~/.emacs.d")

;; autocomplete based on clang

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-clang)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
(define-key ac-mode-map  [(control \;)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang) ac-sources)))
(my-ac-config)

;; standard include directories
;;
; /usr/include/c++/4.6/x86_64-linux-gnu/.
; /usr/include/c++/4.6/backward
; /usr/lib/gcc/x86_64-linux-gnu/4.6/include
; /usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/include/c++/4.6
 /usr/local/include
 /usr/include/x86_64-linux-gnu
 /usr/include
"
               )))

; create a gtags file if none is present
(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
    (let ((olddir default-directory)
          (topdir (read-directory-name  
                    "gtags: top of source tree:" default-directory)))
      (cd topdir)
      (shell-command "gtags && echo 'created tagfile'")
      (cd olddir)) ; restore   
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

(defun my-projects-config ()
  (let ((bname (buffer-file-name)))
    (cond
      ((string-match "SFCGAL/" bname)
       (message "SFCGAL project")
       (setq c-basic-offset 4 )
       (setq ac-clang-flags (append ac-clang-flags '("-I/home/hme/src/SFCGAL/include" "-I/home/hme/libs/include")))
       )

      ((string-match "Quantum-GIS/" bname)
       (message "QGIS project")
       (setq c-basic-offset 2 )
       )

      (t (message "Generic project"))
     )))

(add-hook 'c-mode-common-hook
  (lambda ()
    (require 'gtags)
    (gtags-mode t)
    (my-ac-cc-mode-setup)
    (my-projects-config)
    ;(djcb-gtags-create-or-update)
))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

