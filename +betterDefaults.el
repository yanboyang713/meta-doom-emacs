;;; ../../dotfiles/doom/+betterDefaults.el -*- lexical-binding: t; -*-

;; Set line-number style
(setq display-line-numbers-type 'relative)
;; Exit insert mode by pressing j twice quickly
(setq key-chord-two-keys-delay 0.1)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)
(setq evil-escape-key-sequence nil)

;; doom-mode-line stuff
(setq doom-modeline-enable-word-count t)

;; Set the system time display mode
(setq system-time-locale "C")
;; When opening the file, the cursor is automatically positioned to the position where it stayed last time
(save-place-mode 1)
;; Coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;; No additional confirmation required when closing emacs
(setq confirm-kill-emacs nil)
