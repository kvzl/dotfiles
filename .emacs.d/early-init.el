(setq package-enable-at-startup nil)


;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;;
;; Misc
;;
(setq delete-by-moving-to-trash t)
(setq use-package-verbose t)
(setq inhibit-startup-message t) ;; Disable default startup message
(setq confirm-kill-processes nil)
(setq confirm-kill-emacs #'yes-or-no-p)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls"
	      dired-listing-switches "-aABDhlG --group-directories-first"))

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t)

;; Prevent native-compiling .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

;; Basic appearance settings
;;
(scroll-bar-mode -1)		 ;; Disable visible scrollbar
(tool-bar-mode -1)		 ;; Disable the toolbar
(set-fringe-mode 4)		 ;; Give some breathing room

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq evil-shift-width 2)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		            vterm-mode-hook
		            dired-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq backup-directory-alist `(("." . "~/.saves")))

;; Global key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-`") 'other-frame)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-list-file-prefix nil)
(setq auto-save-default nil)
