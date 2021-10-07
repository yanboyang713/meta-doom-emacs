;;; ../../dotfiles/doom/+research.el -*- lexical-binding: t; -*-

;; helm-bibtex related stuff
(after! helm
  (use-package! helm-bibtex
    :custom
    (bibtex-completion-bibliography '("~/Tools/Zotero/bibtex/library.bib"))
    (reftex-default-bibliography '("~/Tools/Zotero/bibtex/library.bib"))
    (bibtex-completion-pdf-field "file")
    :hook (Tex . (lambda () (define-key Tex-mode-map "\C-ch" 'helm-bibtex))))
  (map! :leader
        :desc "Open literature database"
        "o l" #'helm-bibtex)
  (map! :map helm-map
        "C-j" #'helm-next-line
        "C-k" #'helm-previous-line)
  )
;; org-roam related things
(after! org-roam
  (setq org-roam-directory "~/org/org-roam/")

  (add-hook 'after-init-hook 'org-roam-mode)

  ;; org-roam-bibtex stuff
  (use-package! org-roam-bibtex)
  (org-roam-bibtex-mode)

  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  ;; Let's set up some org-roam capture templates
  (setq org-roam-capture-templates
        (quote (("d" "default" plain
                 "%?"
                 :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                 "#+title: ${title}\n")
                 :unnarrowed t)
                ("r" "bibliography reference" plain
                 (file "~/org/org-roam/templates/orb-capture")
                 :if-new
                 (file+head "references/${citekey}.org" "#+title: ${title}\n")))))

  ;; And now we set necessary variables for org-roam-dailies
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
           "#+title: %<%Y-%m-%d>\n"))))

  ;; Function to capture quotes from pdf
  (defun org-roam-capture-pdf-active-region ()
    (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
           (pdf-buf (get-buffer pdf-buf-name)))
      (if (buffer-live-p pdf-buf)
          (with-current-buffer pdf-buf
            (car (pdf-view-active-region-text)))
        (user-error "Buffer %S not alive." pdf-buf-name)))

  ;; For org-roam-ui
  (use-package! websocket)
  (use-package! org-roam-ui-follow-mode
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t))))

;; For deft
(after! deft
  (setq deft-extensions '("org")
        deft-directory "~/org/org-roam/"
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t))
