(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


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
(set-face-attribute 'default nil :font "Fira Code" :height 130)

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


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))


