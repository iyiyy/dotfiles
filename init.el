;; Add melpa repo
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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
(show-paren-mode 1)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq-default cursor-type 'rectangle)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(setq frame-title-format
      '(buffer-file-name "%b - %f"
                         (dired-directory dired-directory
                                          (revert-buffer-function "%b"
                                                                  "%b - Dir: " default-directory))))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(set-default-font "-bitstream-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1" :antialias=natural)
;;(set-default-font "-ibm-IBM Plex Mono-semibold-normal-normal-*-15-*-*-*-m-0-iso10646-1" :antialias-natural)
(load-theme 'monokai t)

;; Enable line numbers
(global-set-key (kbd "M-p") 'blink-matching-open) ;; Show parent in mini-buffer
(global-set-key (kbd "<f9>") 'linum-mode) ;; toggle line numbers
(global-set-key (kbd "<f8>") 'whitespace-mode) ;;
(global-set-key (kbd "<f5>") 'revert-buffer)
;;(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-.") 'put-file-name-on-clipboard)

;; editor
(setq-default indent-tabs-mode nil) ;; indent with spaces instead of tabs

;; Set a backup directory for temporary files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
)

(add-to-list 'auto-mode-alist '("\\.forge\\'" . python-mode))
