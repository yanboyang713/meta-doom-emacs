;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;

(load! "+org")
(load! "+research")
(load! "+email")
(load! "+misc")
(load! "+bindings")
(load! "+betterDefaults")

(setq user-full-name "Boyang Yan"
      user-mail-address "yanboyang713@gamil.com")

;; Font settings
(setq doom-font (font-spec :family "RobotoMono Nerd Font"  :size 18)
     doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono" :size 15))

;; Theme
(setq doom-theme 'doom-dracula)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; This is to use pdf-tools instead of doc-viewer
;;(use-package! pdf-tools
;;  :config
;;  (pdf-tools-install)
;;  (setq-default pdf-view-display-size 'fit-width)
;;  :custom
;;  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; For textklintrc
(after! flycheck
  (setq flycheck-textlint-config "~/.config/textlint/textlintrc.json")
  (setq flycheck-textlint-executable "~/npm-workspace/node_modules/.bin/textlint")
  )

;; For calendar support
(defun my-open-calendar()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    )))

;; Spelling related
(global-set-key (kbd "C-c N")
                (lambda()(interactive)
                  (ispell-change-dictionary "dutch")
                  (flyspell-buffer)))

;; For inline evaluation of elisp
(eros-mode 1)

;; Adding some new global keys
(map! :leader
      :desc "Open calendar"
      "o c" #'my-open-calendar)

(map! :leader
      :desc "Org noter"
      "n p" #'org-noter)
