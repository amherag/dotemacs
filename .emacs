(add-to-list 'exec-path "/usr/bin/")
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))

                                        ; Loading packages. Needs `use-package` installed.

;; Fixing signature check failure.
;; Uncomment and evaluate this:
;; (setq package-check-signature nil)
;; Then install package: gnu-elpa-keyring-update
;; Finally, uncomment (setq package-check...) and restart.

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(use-package flex-isearch :ensure t)
(use-package nord-theme :ensure t)
(use-package highlight-numbers :ensure t)
(use-package highlight-quoted :ensure t)
(use-package org :ensure t)
(use-package org-bullets :ensure t)
(use-package ox-twbs :ensure t)
(use-package ace-window :ensure t)
;; (use-package recentf :ensure t)
(use-package tramp :ensure t)
(use-package bm :ensure t)
(use-package flycheck :ensure t)
(use-package ledger-mode :ensure t)
(use-package flycheck-ledger :ensure t)
(use-package iedit :ensure t)
;; (use-package neotree :ensure t)
(use-package electric :ensure t)
(use-package go-mode :ensure t)
;; (use-package xah-find :ensure t)
(use-package bison-mode :ensure t)
(use-package spacemacs-theme :defer t :ensure t)
(use-package theme-changer :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package helm-descbinds :ensure t)
;; (use-package disable-mouse :ensure t)
(use-package auctex :defer t :ensure t)
(use-package slime :defer t :ensure t)
;; (use-package sly :defer t :ensure t)
(use-package transpose-frame :defer t :ensure t)
;; (use-package magit :defer t :ensure t)

                                        ; Emacs-specific configuration.

;; Always display in current window.
;; (add-to-list 'display-buffer-alist
;;              '(".*" . display-buffer-same-window))
;; display-buffer-alist

;; save buffer
(global-set-key (kbd "C-x C-x") 'save-buffer)
(global-set-key (kbd "C-x C-s") 'nil)

;; undo
(global-set-key (kbd "C-j") 'undo)

;; fill-region globally
(global-set-key (kbd "C-c C-f") 'fill-region)

;; Using regexp search instead of incremental search.
(setq flex-isearch-mode t)
(global-set-key (kbd "C-s") 'flex-isearch-forward)
(global-set-key (kbd "C-r") 'flex-isearch-backward)

;; Winner mode.
(winner-mode 1)
(global-set-key (kbd "<left>") 'winner-undo)
(global-set-key (kbd "<right>") 'winner-undo)

;; Overriding list-buffers.
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Overriding annoying stuff.
(global-set-key (kbd "M-t") nil)

;; LaTeX configuration.
(setq TeX-PDF-mode t)
(setq latex-run-command "pdflatex")

;; cl includes some required definitions by w3m.
(require 'cl)

;; Line numbers separation.
(setq linum-format "%4d \u2502 ")

;; Visual line mode.
(global-visual-line-mode t)

;; Remove ^M in files containing mixed UNIX and DOS line endings.
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)

;; Top-bottom document navigate.
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)

;; List navigation.
(global-set-key (kbd "M-p") 'backward-list)
(global-set-key (kbd "M-n") 'forward-list)

;; Indentation.
(global-set-key (kbd "M-q") 'indent-pp-sexp)

;; Ergonomic backspace.
(global-set-key (kbd "C-h") (kbd "<backspace>"))

;; Mark defun.
(global-set-key (kbd "M-h") 'mark-defun)

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

;; Display time
(setq display-time-day-and-date t)
(display-time)

;; Display column number.
(setq column-number-mode t)

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
;; (global-unset-key (kbd "C-x C-x"))

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

;; electric-pair-mode.
(electric-pair-mode t)
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) 
    (self-insert-command 1)))

(add-hook 'lisp-mode-hook
          (lambda ()
	    (highlight-quoted-mode t)
	    (highlight-numbers-mode t)
            (define-key lisp-mode-map "\"" 'electric-pair)
            (define-key lisp-mode-map "(" 'electric-pair)
            (define-key lisp-mode-map "[" 'electric-pair)
            (define-key lisp-mode-map "{" 'electric-pair)))

;; org-mode.
(with-eval-after-load 'org
  ;; (define-key org-mode-map [(control c) (control c)] 'org-global-cycle)
  (define-key org-mode-map [(control c) (control n)] 'org-insert-heading)
  (define-key org-mode-map [(control up)] 'org-metaup)
  (define-key org-mode-map [(control down)] 'org-metadown)
  (define-key org-mode-map [(control left)] 'org-metaleft)
  (define-key org-mode-map [(control right)] 'org-metaright)
  (org-bullets-mode t)
  )
(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-twbs)
(add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
(add-to-list 'org-latex-packages-alist '("cache=false" "minted"))
;; NOTE: minted requires pygments to be installed.
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-src-fontify-natively t)
(require 'ob-lisp)
(setq org-latex-prefer-user-labels t)

;; Magit.

;; Transpose frame.
;; (global-set-key (kbd "C-x C-x 1") 'transpose-frame)

;; Settting up slime.
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "ros -Q run")
(setq slime-contribs '(slime-fancy))

(add-hook 'emacs-lisp-mode-hook
	  (defun emacs-lisp-sanitize-bindings ()
	    "Removes emacs lisp's keybindings."
	    (cond ((boundp 'emacs-lisp-mode-map)
		   (define-key emacs-lisp-mode-map (kbd "C-x C-x") 'save-buffer)
		   (define-key emacs-lisp-mode-map (kbd "C-x C-s") 'nil)
		   (message "emacs lisp keybindings have been sanitized"))
		  ('t (message "emacs lisp keybindings not sanitized")))))

(add-hook 'sh-mode-hook
	  (defun shell-sanitize-bindings ()
	    "Removes shell script's keybindings."
	    (cond ((boundp 'sh-mode-map)
		   (define-key sh-mode-map (kbd "C-x C-x") 'save-buffer)
		   (define-key sh-mode-map (kbd "C-x C-s") 'nil)
		   (message "shell script keybindings have been sanitized"))
		  ('t (message "shell script keybindings not sanitized")))))

(add-hook 'go-mode-hook
	  (defun golang-sanitize-bindings ()
	    "Removes Golang's keybindings."
	    (cond ((boundp 'go-mode-map)
		   (define-key go-mode-map (kbd "C-c C-a") nil)
		   (define-key go-mode-map (kbd "C-c C-f") 'fill-region)
		   (message "go-mode keybindings have been sanitized"))
		  ('t (message "go-mode keybindings not sanitized")))))

(add-hook 'slime-mode-hook
	  (defun slime-sanitize-bindings ()
	    "Removes slime's keybindings."
	    (cond ((boundp 'slime-mode-map)
		   ;; Map used by Sly:
		   ;; (define-key sly-editing-mode-map (kbd "M-p") nil)
		   (define-key slime-mode-map (kbd "C-x C-x") 'save-buffer)
		   (define-key slime-mode-map (kbd "C-x C-s") 'nil)
		   (define-key slime-mode-map (kbd "C-c C-f") 'fill-region)
		   (define-key slime-mode-map (kbd "C-c C-a") 'bm-previous)
		   (define-key slime-mode-map (kbd "C-c C-o") 'bm-toggle)
		   (define-key slime-mode-map (kbd "C-c C-e") 'bm-next)
		   (define-key slime-mode-map (kbd "M-p") nil)
		   (define-key slime-mode-map (kbd "M-n") nil)
		   (define-key slime-mode-map (kbd "M-,") nil)
		   (define-key slime-mode-map (kbd "M-.") nil)
		   (message "slime keybindings have been sanitized"))
		  ('t (message "slime keybindings not sanitized")))))

(add-hook 'slime-repl-mode-hook
	  (defun slime-repl-sanitize-bindings ()
	    "Removes slime repl's keybindings."
	    (cond ((boundp 'slime-repl-mode-map)
		   (define-key slime-repl-mode-map (kbd "C-c C-r") 'slime-restart-inferior-lisp)
		   (message "slime repl keybindings have been sanitized"))
		  ('t (message "slime repl keybindings not sanitized")))))

(add-hook 'web-mode-hook
	  (defun web-sanitize-bindings ()
	    "Removes web mode's keybindings."
	    (cond ((boundp 'web-mode-map)
		   (define-key web-mode-map (kbd "C-c C-f") 'fill-region)
		   (define-key web-mode-map (kbd "C-c C-a") 'bm-previous)
		   (define-key web-mode-map (kbd "C-c C-o") 'bm-toggle)
		   (define-key web-mode-map (kbd "C-c C-e") 'bm-next)
		   (message "web mode keybindings have been sanitized"))
		  ('t (message "web mode keybindings not sanitized")))))

(add-hook 'LaTeX-mode-hook
	  (defun tex-sanitize-bindings ()
	    "Removes TeX-latex's keybindings."
	    (cond ((boundp 'LaTeX-mode-map)
		   (define-key LaTeX-mode-map (kbd "C-j") 'undo)
		   (define-key LaTeX-mode-map (kbd "C-c C-o") 'bm-toggle)
		   (define-key LaTeX-mode-map (kbd "C-c C-e") 'bm-next)
		   (define-key LaTeX-mode-map (kbd "C-c C-a") 'bm-previous)
		   (define-key LaTeX-mode-map (kbd "C-c C-f") 'fill-region)

		   (message "TeX-latex's keybindings have been sanitized"))
		  ('t (message "TeX-latex's keybindings not sanitized")))))

;; Visible bookmarks (bm).
;; For terminal environment.
(global-set-key (kbd "C-c o") 'bm-toggle)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-o") 'bm-toggle)
;; For terminal environment.
(global-set-key (kbd "C-c a") 'bm-previous)
;; For non-terminal environment.
(global-set-key (kbd "C-c C-a") 'bm-previous)
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
;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-auto-cleanup 'never)

;; (defun recentf-open-files-compl ()
;;   (interactive)
;;   (let* ((all-files recentf-list)
;; 	 (tocpl (mapcar (function
;; 			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
;; 	 (prompt (append '("Recent File name: ") tocpl))
;; 	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
;;     (find-file (or (cdr (assoc fname tocpl))
;; 		   fname))))

;; (global-set-key "\C-x\C-r" 'recentf-open-files-compl)

;; Use xetex instead of pdftex.
;; (setq latex-run-command "xetex")

;; Show parenthesis.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Neotree.
;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-smart-open t)

;; Flycheck.
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Iedit.
(require 'iedit)
(global-set-key (kbd "C-d") 'iedit-mode)
(put 'narrow-to-region 'disabled nil)

;; Disable mouse.
;; (global-disable-mouse-mode)

;; Rainbow-delimiters for major programming modes.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Automatically change theme depending on daytime.
(setq calendar-location-name "Tijuana, MX")
(setq calendar-latitude 32.534851)
(setq calendar-longitude -117.043457)
(require 'theme-changer)
(change-theme 'nord 'nord)

                                        ; Extra.

;; Use Golang syntax with CXGO programs
(add-to-list 'auto-mode-alist '("\\.cx\\'" . go-mode))

;; Always use web-mode for .html, .js and .css files.
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; Copy `diff` results without `-` or `+` at the beginning of line.
(defun copy-diff-region ()
  "Copy diff region without + or - markers."
  (interactive)
  (deactivate-mark)
  (let ((text (buffer-substring-no-properties
               (region-beginning) (region-end))))
    (kill-new (replace-regexp-in-string "^[\\+\\-]" "" text))))

(global-set-key (kbd "C-x M-w") 'copy-diff-region)

(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(custom-enabled-themes (quote (nord)))
 '(custom-safe-themes
   (quote
    ("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" default)))
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
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (flex-isearch slime zerodark-theme zenburn-theme yasnippet xah-find web-mode w3m use-package transpose-frame toml-mode theme-changer swiper subatomic256-theme subatomic-theme srcery-theme spacemacs-theme smartparens simpleclip redshank rainbow-identifiers rainbow-delimiters ox-twbs ox-mediawiki org-bullets org nyx-theme nord-theme neotree monokai-theme monokai-pro-theme monokai-alt-theme markdown-mode macrostep ledger-mode iedit highlight-thing highlight-quoted highlight-numbers helm-go-package helm-descbinds go-rename go-guru go-eldoc gnu-elpa-keyring-update flycheck-ledger f discover-my-major disable-mouse darkokai-theme cyberpunk-theme command-log-mode codesearch cider chronos bm bison-mode bash-completion badwolf-theme autopair auto-dictionary auctex atom-one-dark-theme atom-dark-theme arjen-grey-theme apropospriate-theme anaphora ample-zen-theme ample-theme ahungry-theme afternoon-theme ace-window abyss-theme)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#262626")))
 '(pop-up-frames nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "default" :foundry "default" :slant normal :weight normal :height 100 :width normal)))))



(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [f12] 'toggle-window-dedicated)
