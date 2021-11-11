(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defun put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; utils
(setq ring-bell-function 'ignore) ;; disable alarm bell
(windmove-default-keybindings 'meta) ;; Enable M-<left | up | right | down> for selecting windows

;; Aesthetic settings
(global-linum-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq-default cursor-type 'rectangle)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(setq frame-title-format '("%b"))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(load-theme 'doom-one t)

;; Enable line numbers
(global-set-key (kbd "M-p") 'blink-matching-open) ;; Show parent in mini-buffer
(global-set-key (kbd "<f9>") 'linum-mode) ;; toggle line numbers
(global-set-key (kbd "<f7>") 'whitespace-mode) ;;
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-.") 'put-file-name-on-clipboard)

;; editor
(setq-default indent-tabs-mode nil) ;; indent with spaces instead of tabs
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Set a backup directory for temporary files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      )

