;; Add melpa repo
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/my_packages/")
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

(set-default-font "-bitstream-Hack-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1" :antialias=natural)
(load-theme 'monokai-pro t)

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

;;(global-company-mode 1)
;;(company-quickhelp-mode)
;;setq company-idle-delay 0.1)

(use-package company
  :diminish company-mode
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay            0.1
        company-minimum-prefix-length 2
        company-show-numbers          t
        company-tooltip-limit         20
        company-dabbrev-downcase      nil
        company-backend               '((company-gtags))
        )
  :bind ("s-;" . company-complete-common)
  )

;; C mode
(setq c-default-style "k&r"
      c-basic-offset 4)

;; Set a backup directory for temporary files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      )

(require 'xuda-mode)
(add-to-list 'auto-mode-alist '("\\.forge\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.xuda\\'" . xuda-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-mode t)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
        (quote
         ("1d2f406a342499f0098f9388b87d05ec9b28ccb12ca548f4f5fa80ae368235b6" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "fc86ef89d1bf3ea2b7da1d3e944ff86b6296cd1a5b6227a73f5191842055c45f" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "4b19d61c560a93ef90767abe513c11f236caec2864617d718aa366618133704c" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" default)))
 '(global-company-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
        (quote
         (use-package company-quickhelp company monokai-pro-theme parchment-theme apropospriate-theme birds-of-paradise-plus-theme brutalist-theme markdown-mode centered-cursor-mode wiki-summary scala-mode dash flycheck tabbar neotree monokai-theme monokai-alt-theme)))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
        (quote
         ((20 . "#F92672")
          (40 . "#CF4F1F")
          (60 . "#C26C0F")
          (80 . "#E6DB74")
          (100 . "#AB8C00")
          (120 . "#A18F00")
          (140 . "#989200")
          (160 . "#8E9500")
          (180 . "#A6E22E")
          (200 . "#729A1E")
          (220 . "#609C3C")
          (240 . "#4E9D5B")
          (260 . "#3C9F79")
          (280 . "#A1EFE4")
          (300 . "#299BA6")
          (320 . "#2896B5")
          (340 . "#2790C3")
          (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
        (quote
         (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
