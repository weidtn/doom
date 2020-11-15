(load-file "~/.config/doom/local.el")
(setq user-full-name "Nikolai Weidt")
(setq user-mail-address "aramus92@gmail.com")

(setq doom-font (font-spec :family "hack" :size 14))

(setq display-line-numbers-type 'relative)

(use-package! zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(setq calendar-week-start-day 1
          calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                   "Donnerstag" "Freitag" "Samstag"]
          calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                     "Juni" "Juli" "August" "September"
                                     "Oktober" "November" "Dezember"])

(after! org
  (setq org-bullets-bullet-list '("✖" "✚")
        org-image-actual-width  300
        org-preview-latex-default-process 'dvisvgm
        org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
        org-latex-prefer-user-labels t
        org-agenda-files '("~/org/" "~/org/roam/")
        org-ellipsis "▼")
  (setq org-latex-packages-alist
        '(("version=4" "mhchem")
          ("separate-uncertainty, exponent-product = \\cdot" "siunitx"))))

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("scrartcl" "\\documentclass[parskip]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package! org-ref
  :after org
;;  :init
 :config
 (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
 (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

;; for helm-bibtex as completion method
 (setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
      bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))
)

(setq org-babel-python-command "python3")
(after! org-babel
  (org-babel-jupyter-override-src-block "python"))

(setq org-babel-clojure-backend 'cider)

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))



(after! org-roam
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert-immediate" "I" #'org-roam-insert-immediate
        :desc "org-roam-capture" "c" #'org-roam-capture
        ;; :desc "org-journal-new-entry" "j" #'org-journal-new-entry))
        ))
;; deft for browsing notes
(setq deft-recursive t
      ;;       deft-use-filter-string-for-filename t
      ;;       deft-default-extension "org"
      deft-directory "~/org/roam/")
;; ;; org-journal for dailies
(setq org-journal-date-prefix "#+title: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-dir "~/org/roam/"
      org-journal-time-format ""
      org-journal-date-format "%A, %d %B %Y")
(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))
(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-field t
      orb-file-field-extensions "pdf")

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:")))

(use-package! platformio-mode
:config (add-to-list 'company-backends 'company-irony)
)

(use-package! irony
:config
(add-to-list 'irony-supported-major-modes 'arduino-mode)
(add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++")))

(add-hook! arduino-mode #'irony-mode 'irony-eldoc 'platformio-conditionally-enable)

(add-hook! irony-mode
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)

  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)

  (irony-cdb-autosetup-compile-options))

(add-hook! flycheck-mode 'flycheck-irony-setup)

(after! python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(setq geiser-active-implementations '(chicken))
(setq geiser-chicken-binary "/usr/bin/chicken-csi")

(set-email-account! "aramus92@gmail.com"
                    '(
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-smtp-user. "aramus92@gmail.com")
                      (smtpmail-smtp-service  . 587)
                      (smtpmail-stream-type   . starttls)
                      (smtpmail-debug-info    . t)
                      (mu4e-sent-folder . "/Gesendet")
                      (mu4e-drafts-folder . "/Entwürfe")
                      (mu4e-trash-folder . "/Papierkorb")
                      (mu4e-refile-folder . "/Alle")
                      (mu4e-compose-signature . "\nBest Regards\n\nNikolai Weidt")
                      (mu4e-update-interval . 60) ;; sec
                      )
                    t)
(setq smtpmail-auth-credentials (expand-file-name "~/.emacs.d/mu4e/.mbsyncpass-gmail.gpg"))

(after! mu4e
  (setq mu4e-maildir-shortcuts
        '( (:maildir "/INBOX"              :key ?i)
           (:maildir "/Gesendet"  :key ?s)
           (:maildir "/Papierkorb"      :key ?t)
           (:maildir "/Alle"   :key ?a)))
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications)
  (add-to-list 'mu4e-bookmarks
               '(:name "Test"
                 :query "flag:unread AND maildir:/INBOX"
                 :key ?b))
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND maildir:/INBOX"))
  )

(use-package! helm-bibtex
  :config
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-pdf-open-function 'find-file))


