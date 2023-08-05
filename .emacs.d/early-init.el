;;; -*- no-byte-compile: t -*-

(defun kvzl/dashboard-init-info ()
  (let ((package-count 0))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
    (when (fboundp 'elpaca--queued)
      (setq package-count (length (elpaca--queued))))
    (format "%d packages loaded in %s with %d garbaged collections."
            package-count
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time before-init-time)))
            gcs-done)))

(setq dashboard-init-info 'kvzl/dashboard-init-info)

;;
;; Global key bindings
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; Make ESC quit prompts
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

(defun open-user-config ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(bind-key "M-," 'open-user-config override-global-map)

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
	      dired-listing-switches "-aBhl --group-directories-first"))

(setq frame-resize-pixelwise t) 
(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t)

;; Prevent native-compiling .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

(when (or (boundp 'comp-eln-load-path) (boundp 'native-comp-eln-load-path))
  (let ((eln-cache-dir (expand-file-name "cache/eln-cache/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))

    (if (boundp 'native-comp-eln-load-path)
        (setcar native-comp-eln-load-path eln-cache-dir)
      (setcar comp-eln-load-path eln-cache-dir))
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete" "-or"
                    "-name" "*.eln.tmp" "-size" "0" "-delete"))))

;;
;; Basic appearance settings
;;
(scroll-bar-mode -1)		 ;; Disable visible scrollbar
(tool-bar-mode -1)		 ;; Disable the toolbar
(set-fringe-mode 4)		 ;; Give some breathing room

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq evil-shift-width 2)

;; Fonts
(setq-default line-spacing 2)
;; (set-face-attribute 'default nil :font "Jetbrains Mono" :height 120)  ;; should be called after initializing ligature.el

(let ((frame-transparency '(90 . 90)))
  (set-frame-parameter (selected-frame) 'alpha frame-transparency)
  (add-to-list 'default-frame-alist `(alpha . ,frame-transparency)))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		            vterm-mode-hook
		            dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq backup-directory-alist `(("." . "~/.saves")))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

