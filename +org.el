;;; ../../dotfiles/doom/+org.el -*- lexical-binding: t; -*-

;; org-mode related stuff
(after! org
  ;; Set org directories
  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/refile.org")
  (setq org-agenda-files (quote("~/org/"
                                "~/org/synced/"
                                "~/org/org-roam/"
                                "~/org/org-roam/daily/"
                                "~/org/org-roam/references/"
                                )))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-ellipsis " ▼ ")
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)

  ;; org keyword related stuff
  (setq org-todo-keywords
        (quote ((sequence
                 "TODO(t)"
                 "PROJ(p)"
                 "LOOP(r)"
                 "STRT(s)"
                 "IDEA(i)"
                 "NEXT(n)"
                 "|"
                 "DONE(d)")
                (sequence
                 "WAIT(w@/!)"
                 "HOLD(h@/!)"
                 "|"
                 "KILL(k@/!)")
                (sequence
                 "[ ](T)"
                 "[-](S)"
                 "[?](W)"
                 "|"
                 "[X](D)"
                 ))))

  (setq org-todo-keyword-faces
        (quote (
                ("NEXT" +-lock-constant-face bold))))

  (setq org-todo-state-tags-triggers
        (quote (("KILL" ("KILL" . t))
                ("WAIT" ("WAIT" . t))
                ("HOLD" ("WAIT") ("HOLD" . t))
                (done ("WAIT") ("HOLD"))
                ("TODO" ("WAIT") ("KILL") ("HOLD"))
                ("NEXT" ("WAIT") ("KILL") ("HOLD"))
                ("DONE" ("WAIT") ("KILL") ("HOLD")))))

  ;; org capture related stuff
  (setq org-capture-templates
        (quote (("r" "respond" entry (file+headline "~/org/refile.org" "Emails")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
                ("p" "project" entry (file+headline "~/org/refile.org" "Projects")
                 "* PROJ %?\n%U\n%a\n")
                ("t" "todo" entry (file+headline "~/org/refile.org" "Tasks")
                 "* TODO %?\nSCHEDULED: %t\n%U\n%a\n")
                ("i" "idea" entry (file+headline "~/org/refile.org" "Ideas")
                 "* IDEA %?\n%U\n%a\n")
                ("e" "external" entry (file+headline "~/org/refile.org" "External")
                 "* TODO %?\nSCHEDULED: %t\n%U\n%a\n %(progn (setq kk/delete-frame-after-capture 1) \"\")")
                )))

  ;; Caldav sync
  (setq diary-location "~/.local/share/diary/")

  (setq calendars
        '(("outlook" . "http://localhost:1080/users/45995wsp@eur.nl/calendar/")
          ))

  (defun getcal (url file)
    "Download ics file and add it to file"
    (let ((tmpfile (url-file-local-copy url)))
      (icalendar-import-file tmpfile file)
      (kill-buffer (car (last (split-string tmpfile "/"))))))

  (defun getcals ()
    "Load a set of ICS calendars into Emacs diary files"
    (interactive)
    (mapcar #'(lambda (x)
                (let ((file (concat diary-location (car x)))
                      (url (cdr x)))
                  (message (concat "Loading " url " into " file))
                  (find-file file)
                  ;; (flush-lines "^[& ]") ;; if you import ical as non marking
                  (erase-buffer) ;; to avoid duplicating events
                  (getcal url file)
                  ))
            calendars))

  (setq org-agenda-include-diary t)
  (setq diary-file "~/.local/share/diary/outlook")

  ;; Kill capture frame
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (defun kk/delete-frame-if-neccessary (&rest r)
    (cond
     ((= kk/delete-frame-after-capture 0) nil)
     ((> kk/delete-frame-after-capture 1)
      (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
     (t
      (setq kk/delete-frame-after-capture 0)
      (delete-frame))))

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

  ;; org refile related stuff
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)
                                   ("~/org/org-roam/" :maxlevel . 9))))

  (setq org-refile-use-outline-path t)

  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (defun ws/verify-refile-target ()
    "Eclude todo keywords with a done state"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  ;; Prevent adding org agenda files
  (map! :map org-mode-map "C-c [" nil)

  ;; Set up org-ref stuff
  (use-package! org-ref
    :custom
    (org-ref-default-bibliography "/home/wouter/Tools/Zotero/bibtex/library.bib")
    (org-ref-default-citation-link "citep"))

  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file pdf-file)
        (message "No PDF found for %s" key))))

  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-export-latex-format-toc-function 'org-export-latex-no-toc
        org-ref-get-pdf-filename-function
        (lambda (key) (car (bibtex-completion-find-pdf key)))
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point
        ;; For pdf export engines
        org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -bibtex -f -output-directory=%o %f")
        org-ref-notes-function 'orb-edit-notes)

  ;; Set up org-mode export stuff
  (setq org-latex-to-mathml-convert-command
        "java -jar %j -unicode -force -df %o %I"
        org-latex-to-mathml-jar-file
        "/home/wouter/Tools/math2web/mathtoweb.jar")

  (add-to-list 'org-latex-classes
               '("apa6"
                 "\\documentclass{apa6}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass{memoir}"
                 ("\\book{%s}" . "\\book*{%s}")
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s} .\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("paper"
                 "\\documentclass{paper}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))

  ;; org-noter stuff
  (after! org-noter
    (setq
     org-noter-notes-search-path "~/org/org-roam/references/"
     org-noter-hide-other nil
     org-noter-separate-notes-from-heading t
     org-noter-always-create-frame nil)
    (map!
     :map org-noter-doc-mode-map
     :leader
     :desc "Insert note"
     "m i" #'org-noter-insert-note
     :desc "Insert precise note"
     "m p" #'org-noter-insert-precise-note
     :desc "Go to previous note"
     "m k" #'org-noter-sync-prev-note
     :desc "Go to next note"
     "m j" #'org-noter-sync-next-note
     :desc "Create skeleton"
     "m s" #'org-noter-create-skeleton
     :desc "Kill session"
     "m q" #'org-noter-kill-session
     )
    )
  )
