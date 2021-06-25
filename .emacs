(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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

(use-package web-mode :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))
(use-package ag :ensure t)
(use-package solidity-mode :ensure t)
(use-package js2-mode :ensure t)
(use-package js2-refactor :ensure t)
(use-package xref-js2 :ensure t)
(use-package vue-mode :ensure t)
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
(use-package neotree :ensure t)
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
(use-package vue-mode :ensure t)
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

;; line numbers emacs > 26
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


;; flyspell aspell bin
(setq ispell-program-name "/usr/bin/aspell")

;; open file in current buffer.
(add-to-list 'display-buffer-alist
               '("^[^\\*].*[^\\*]$" display-buffer-same-window) t)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Always display in current window.
;; (add-to-list 'display-buffer-alist
;;              '(".*" . display-buffer-same-window))
;; display-buffer-alist

;; copy and paste for WSL.
(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe"))

(defun wsl-paste ()
  (interactive)
  (let ((wslbuffername "wsl-temp-buffer"))
    (get-buffer-create wslbuffername)
    (with-current-buffer wslbuffername
      (insert (let ((coding-system-for-read 'dos))
		;; TODO: put stderr somewhere else
		(shell-command "powershell.exe -command 'Get-Clipboard' 2> /dev/null" wslbuffername nil))))
    (insert-buffer wslbuffername)
    (kill-buffer wslbuffername)))


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
;; (require 'cl)

;; Line numbers separation.
;; (setq linum-format "%4d \u2502 ")

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
;; (global-linum-mode 1)

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
(set-cursor-color "#000000")

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
(global-set-key (kbd "<f5>") 'wsl-copy)
(global-set-key (kbd "<f6>") 'wsl-paste)

;; Changing set-mark.
(global-set-key (kbd "C-t") 'set-mark-command)

;; (Un)Commenting regions.
(global-set-key (kbd "C-x C-q") 'comment-region)
(global-set-key (kbd "C-x C-j") 'uncomment-region)

                                        ; Package-specific configuration.

;; electric-pair-mode.
(electric-indent-just-newline t)
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
(setq org-todo-keywords
      '((sequence "TODO" "PROBLEM" "CONJECTURE" "RESEARCH" "DISCUSS" "|" "DONE" "FACT" "CMD" "SOLUTION" "DISCUSSED")))


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
(setq inferior-lisp-program "ros dynamic-space-size=4096 -Q run")
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
                   (setq web-mode-markup-indent-offset 2)
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

		   (flyspell-mode 1)

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
;; (global-disable-mouse-mode)

;; Rainbow-delimiters for major programming modes.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Automatically change theme depending on daytime.
(setq calendar-location-name "Tijuana, MX")
(setq calendar-latitude 32.534851)
(setq calendar-longitude -117.043457)
(require 'theme-changer)
(change-theme 'enlightened 'enlightened)

                                        ; Extra.

;; js-mode indentation spaces.
(setq js-indent-level 4)
(setq indent-tabs-mode nil)


;; Use Golang syntax with CXGO programs
(add-to-list 'auto-mode-alist '("\\.cx\\'" . go-mode))

;; js2
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; (define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))



;; Always use web-mode for .html, .js and .css files.
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#080808" "#d70000" "#67b11d" "#875f00" "#268bd2" "#af00df" "#00ffff" "#b2b2b2"])
 '(custom-enabled-themes '(enlightened))
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "8e79884e89740cf6b7e0210f52e4ac995dc1f1a9a17151bfcfb4d660757a011b" "f6cdb429a64db06d3db965871b45ed1c666fdce2d3e2c4b810868e4cf4244c92" "7c20c453ad5413b110ccc3bb5df07d69999d741d29b1f894bd691f52b4abdd31" "2476a71deab2ff893bc46bc3f958f084969ce269ee634ca17d2e8647bb1183f2" "9375315e4786e5cc84b739537102802c18650f3168cf7c29f7fbb00a54f9b8e0" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "ae4aa4bf7418af9a2a8a0e9d172895a2f25fe725790fed3f259bba53159a8264" "76c36aaf67479c7b65aba53988ae28f7f0fc386d0e6ec26ee2459061ef232a35" "bcd0237b2a5b7897e482458cc62c4f3fa3d9d7f9a9667338e67d4c7a8e009819" "454c1c9ce70f7d807c51c890910365fd3c64a9e63f596511e9ff57dd97bbeea8" "3263bd17a7299449e6ffe118f0a14b92373763c4ccb140f4a30c182a85516d7f" "5eed5311ae09ed84cb2e4bf2f033eb4df27e7846a68e4ea3ab8d28f6b017e44a" "f4158db802ae689ed0e156cd02c8a3c0e22c5e778578e8eea6d4afc3a9d0e629" "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" default))
 '(fci-rule-color "#444444")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
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
     ("\\?\\?\\?+" . "#dc752f")))
 '(lsp-diagnostics-attributes
   '((unnecessary :foreground "cornsilk4")
     (deprecated :strike-through t)))
 '(package-selected-packages
   '(window-purpose dedicated ag solidity-mode js2-refactor xref-js2 js2-mode eink-theme enlightened-theme espresso-theme exotica-theme eziam-theme faff-theme fantom-theme farmhouse-theme firecode-theme flatfluc-theme flatland-theme flatui-dark-theme flatui-theme flucui-themes foggy-night-theme forest-blue-theme vue-mode flex-isearch slime zerodark-theme zenburn-theme yasnippet xah-find web-mode w3m use-package transpose-frame toml-mode theme-changer swiper subatomic256-theme subatomic-theme srcery-theme spacemacs-theme smartparens simpleclip redshank rainbow-identifiers rainbow-delimiters ox-twbs ox-mediawiki org-bullets org nyx-theme nord-theme neotree monokai-theme monokai-pro-theme monokai-alt-theme markdown-mode macrostep ledger-mode iedit highlight-thing highlight-quoted highlight-numbers helm-go-package helm-descbinds go-rename go-guru go-eldoc gnu-elpa-keyring-update flycheck-ledger f discover-my-major disable-mouse darkokai-theme cyberpunk-theme command-log-mode codesearch cider chronos bm bison-mode bash-completion badwolf-theme autopair auto-dictionary auctex atom-one-dark-theme atom-dark-theme arjen-grey-theme apropospriate-theme anaphora ample-zen-theme ample-theme ahungry-theme afternoon-theme ace-window abyss-theme))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#262626"))
 '(pop-up-frames nil)
 '(vc-annotate-background "#444444")
 '(vc-annotate-color-map
   '((20 . "#ff0000")
     (40 . "#ff4a52")
     (60 . "#f6aa11")
     (80 . "#f1e94b")
     (100 . "#f5f080")
     (120 . "#f6f080")
     (140 . "#41a83e")
     (160 . "#40b83e")
     (180 . "#b6d877")
     (200 . "#b7d877")
     (220 . "#b8d977")
     (240 . "#b9d977")
     (260 . "#93e0e3")
     (280 . "#72aaca")
     (300 . "#8996a8")
     (320 . "#afc4db")
     (340 . "#cfe2f2")
     (360 . "#dc8cc3")))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(when
      (or
       (not
	(boundp 'ansi-term-color-vector))
       (not
	(facep
	 (aref ansi-term-color-vector 0))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "light gray" :foreground "pale green")))))



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
