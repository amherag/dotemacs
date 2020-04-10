                                        ; Loading packages. Needs `use-package` installed.

(package-initialize)

;; Fixing signature check failure.
;; Uncomment and evaluate this:
;; (setq package-check-signature nil)
;; Then install package: gnu-elpa-keyring-update
;; Finally, uncomment (setq package-check...) and restart.


(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/")
 t)

(use-package ace-window :ensure t)
(use-package recentf :ensure t)
(use-package tramp :ensure t)
(use-package bm :ensure t)
(use-package autopair :ensure t)
(use-package flycheck :ensure t)
(use-package ledger-mode :ensure t)
(use-package flycheck-ledger :ensure t)
(use-package iedit :ensure t)
(use-package neotree :ensure t)
(use-package go-mode :ensure t)
(use-package xah-find :ensure t)
(use-package bison-mode :ensure t)
(use-package spacemacs-theme :defer t :ensure t)
(use-package theme-changer :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package disable-mouse :ensure t)
(use-package auctex :defer t :ensure t)
(use-package slime :defer t :ensure t)
(use-package transpose-frame :defer t :ensure t)
(use-package magit :defer t :ensure t)

                                        ; Emacs-specific configuration.

;; Visual line mode.
(global-visual-line-mode t)

;; Remove ^M in files containing mixed UNIX and DOS line endings.
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)

;; Ergonomic backspace.
(global-set-key (kbd "C-h") (kbd "<backspace>"))

;;; Prefer backward-kill-word over backspace.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Comment region.
(global-set-key (kbd "C-c #") 'comment-region)
;; Uncomment region.
(global-set-key (kbd "C-c $") 'uncomment-region)

;; Override mail.
(global-set-key (kbd "C-x m") 'execute-extended-command)

;; Disable evil auto copy selected text.
;; (delete-selection-mode)
;; (setq x-select-enable-primary nil)
;; (setq x-select-enable-clipboard nil)

;; Show OS menu bar.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Linum.
(global-linum-mode 1)

;; Recenter.
;; (global-set-key (kbd "C-'") 'recenter)

;;Display time
(setq display-time-day-and-date t)
(display-time)

;; Change font size
(setq resize-mini-windows nil)
(set-face-attribute 'default nil :height 100)

;; Font
(set-frame-font "Consolas 13" nil t)

;; Set Locale
(setenv "LANG" "en_US.UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Avoid asking "are you sure you want to save?"
(defun ask-user-about-supersession-threat (fn)
  "Blatantly ignore files that changed on disk."
  )

(defun ask-user-about-lock (file opponent)
  "Always grab lock."
  t)

;; ?? in line number.
(setq line-number-display-limit-width 2000000)

;; Insert date in text
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
		   ;;((not prefix) "%a %b %d  %H:%M:%S")
		   ((not prefix) "[%Y-%m-%d %a %H:%M]")
		   ((equal prefix '(4)) "%Y-%m-%d")
		   ((equal prefix '(16)) "%A, %d. %B %Y")))
	  (system-time-locale "en_US"))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; Cursor type and color
(setq-default cursor-type 'box)
(set-cursor-color "#fff")

;; Automatically refresh files/buffers.
(global-auto-revert-mode t)

;; Cursor delay.
(setq blink-cursor-delay 0)

;; Auto-balance buffers.
(defadvice split-window-below (after restore-balanace-below activate)
  (balance-windows))

(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))

(defadvice delete-window (after restore-balance activate)
  (balance-windows))

;; Open in same window.
(add-to-list 'same-window-buffer-names "*xah-find output*")
(add-to-list 'same-window-buffer-names "*shell*")
(add-to-list 'same-window-buffer-names "*grep*")

;; Temporarily maximize a buffer fullscreen.
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
	   (progn
	     (jump-to-register '_)
	     (balance-windows))
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows)
	   (balance-windows))))

(global-set-key (kbd "<f9>") 'toggle-maximize-buffer)

;; Disable splash screen and startup message.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable mark ring.
(setq mark-ring-max 0)
(setq global-mark-ring-max 0)
(global-unset-key (kbd "C-x C-x"))

;; Problem with certain fonts.
(setq inhibit-compacting-font-caches t)

;; Lemonade; clipboard client.
(defun lemonade-copy (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "lemonade copy")
  )
(defun lemonade-paste ()
  (interactive)
  (insert (shell-command-to-string "lemonade paste"))
  )
(global-set-key (kbd "<f5>") 'lemonade-copy)
(global-set-key (kbd "<f6>") 'lemonade-paste)

;; Changing set-mark.
(global-set-key (kbd "C-t") 'set-mark-command)

;; (Un)Commenting regions.
(global-set-key (kbd "C-x C-q") 'comment-region)
(global-set-key (kbd "C-x C-j") 'uncomment-region)

                                        ; Package-specific configuration.

;; Magit.

;; Transpose frame.
(global-set-key (kbd "C-x C-x 1") 'transpose-frame)

;; Settting up slime.
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Visible bookmarks (bm).
;; For terminal environment.
(global-set-key (kbd "C-c a") 'bm-toggle)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-a") 'bm-toggle)
;; For terminal environment.
(global-set-key (kbd "C-c o") 'bm-previous)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-o") 'bm-previous)
;; For terminal environment.
(global-set-key (kbd "C-c e") 'bm-next)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-e") 'bm-next)


;; Ace-window.
(global-set-key (kbd "C-x C-o") 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Tramp enables you to use sudo
(require 'tramp)
(setq tramp-default-method "scp")

;; Saves a list of recent opened files
(require 'recentf)
(recentf-mode 1)
(setq recentf-auto-cleanup 'never)

(defun recentf-open-files-compl ()
  (interactive)
  (let* ((all-files recentf-list)
	 (tocpl (mapcar (function
			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
	 (prompt (append '("Recent File name: ") tocpl))
	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
    (find-file (or (cdr (assoc fname tocpl))
		   fname))))

(global-set-key "\C-x\C-r" 'recentf-open-files-compl)

;; Use xetex instead of pdftex.
;; (setq latex-run-command "xetex")

;; Always use autopair.
(autopair-global-mode 1)

;; Show parenthesis.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Neotree.
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

;; Flycheck.
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Iedit.
(require 'iedit)
(global-set-key (kbd "C-d") 'iedit-mode)
(put 'narrow-to-region 'disabled nil)

;; Disable mouse.
(global-disable-mouse-mode)

;; Rainbow-delimiters for major programming modes.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Automatically change theme depending on daytime.
(setq calendar-location-name "Tijuana, MX")
(setq calendar-latitude 32.534851)
(setq calendar-longitude -117.043457)
(require 'theme-changer)
(change-theme 'leuven 'spacemacs-dark)

                                        ; Extra.

;; Use Golang syntax with CXGO programs
(add-to-list 'auto-mode-alist '("\\.cx\\'" . go-mode))

;; Copy `diff` results without `-` or `+` at the beginning of line.
(defun copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

(global-set-key (kbd "C-x M-w") 'copy-diff-region)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1C1B19" "#EF2F27" "#519F50" "#FED06E" "#2C78BF" "#E02C6D" "#0AAEB3" "#4E4E4E"])
 '(beacon-color "#F8BBD0")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("d986619578e8a8dabb846e91c54090b82d937672f54ffa0ef247c0428813d602" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "3cd4f09a44fe31e6dd65af9eb1f10dc00d5c2f1db31a427713a1784d7db7fdfc" "d7383f47263f7969baf3856ab8b3df649eb77eafdff0c5731bee2ad18e0faed2" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a" "144f05e2dfa7a7b50cad0c3519498ac064cc9da1f194b8ea27d0fb20129d8d7a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "174502267725776b47bdd2d220f035cae2c00c818765b138fea376b2cdc15eb6" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-color "#121212")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2aa198")
     ("PROG" . "#268bd2")
     ("OKAY" . "#268bd2")
     ("DONT" . "#d70000")
     ("FAIL" . "#d70000")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#875f00")
     ("KLUDGE" . "#875f00")
     ("HACK" . "#875f00")
     ("TEMP" . "#875f00")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")
     ("???" . "#dc752f"))))
 '(magit-diff-use-overlays nil)
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (magit nyx-theme transpose-frame gnu-elpa-keyring-update slime auctex flycheck-ledger bm zerodark-theme zenburn-theme yasnippet xah-find web-mode use-package toml-mode theme-changer subatomic256-theme subatomic-theme srcery-theme spacemacs-theme smartparens simpleclip rainbow-identifiers rainbow-delimiters nord-theme neotree monokai-theme monokai-pro-theme monokai-alt-theme markdown-mode ledger-mode iedit helm-go-package helm go-rename go-guru go-eldoc flycheck f disable-mouse darkokai-theme cyberpunk-theme codesearch cider bison-mode bash-completion badwolf-theme autopair auto-dictionary atom-one-dark-theme atom-dark-theme arjen-grey-theme apropospriate-theme anaphora ample-zen-theme ample-theme ahungry-theme afternoon-theme ace-window abyss-theme)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#262626")))
 '(pos-tip-background-color "#ffffff")
 '(pos-tip-foreground-color "#78909C")
 '(tabbar-background-color "#ffffff")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "goldenrod")
     (60 . "#e7c547")
     (80 . "DarkOliveGreen3")
     (100 . "#70c0b1")
     (120 . "DeepSkyBlue1")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "goldenrod")
     (200 . "#e7c547")
     (220 . "DarkOliveGreen3")
     (240 . "#70c0b1")
     (260 . "DeepSkyBlue1")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "goldenrod")
     (340 . "#e7c547")
     (360 . "DarkOliveGreen3"))))
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
