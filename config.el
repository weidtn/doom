(load-file "~/.config/doom/local.el")
(setq user-full-name "Nikolai Weidt")
(setq user-mail-address "aramus92@gmail.com")

(setq doom-font (font-spec :family "hack" :size 14))

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-zenburn)

(setq calendar-week-start-day 1
          calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                                   "Donnerstag" "Freitag" "Samstag"]
          calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                     "Juni" "Juli" "August" "September"
                                     "Oktober" "November" "Dezember"])

(doom-load-envvars-file "~/.config/doom/myenv")

(use-package! org
 :config
 (setq org-bullets-bullet-list '("✖" "✚")
       org-image-actual-width  300
       org-preview-latex-default-process 'dvisvgm
       org-latex-listings 'minted
       org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
       org-latex-prefer-user-labels t
       ;;org-agenda-files '("~/org/" "~/org/roam/")
       org-ellipsis "▼")

;;  babel
;; (setq org-babel-python-command "python3")
;; (setq org-babel-clojure-backend 'cider)
)

;; (org-babel-jupyter-override-src-block "python") ;; apparently this is not a function anymore

(after! ox-latex
 (setq org-latex-packages-alist
       '(("version=4" "mhchem")
         ("separate-uncertainty, exponent-product = \\cdot" "siunitx")))
 ;; koma script scrartcl:
 (add-to-list 'org-latex-classes
              '("scrartcl" "\\documentclass[parskip]{scrartcl}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after! org-download
 (setq org-download-screenshot-method "flameshot gui --raw > %s"))

(after! org-ref
  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib")
        bibtex-completion-library-path '("~/Dropbox/emacs/bibliography/bibtex-pdfs/")
        bibtex-completion-notes-path "~/Dropbox/emacs/bibliography/notes/"
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath
                   (call-process "open" nil 0 nil fpath))))

 (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra)
 (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
       ;; org-ref-insert-cite-function 'org-ref-cite-insert-helm
       org-ref-insert-label-function 'org-ref-insert-label-link
       org-ref-insert-ref-function 'org-ref-insert-ref-link
       org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

;; colors in jupyter even with superstar
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))



(after! org-roam
 (map! :leader
       :prefix "n"
       :desc "org-roam" "l" #'org-roam
       :desc "org-roam-insert" "i" #'org-roam-insert
       :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
       :desc "org-roam-node-find" "f" #'org-roam-node-find
       :desc "org-roam-show-graph" "g" #'org-roam-show-graph
       :desc "org-roam-insert-immediate" "I" #'org-roam-insert-immediate
       :desc "org-roam-capture" "c" #'org-roam-capture)
       ;; :desc "org-journal-new-entry" "j" #'org-journal-new-entry))
 (org-roam-db-autosync-mode +1)
 (setq org-roam-directory "~/org/roam/")
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
       org-journal-date-format "%A, %d %B %Y"))

(use-package! org-roam-bibtex
 :after org-roam
 :hook (org-roam-mode . org-roam-bibtex-mode)
 :config
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
:END:"))))

(setq completion-styles '(orderless)
      completion-category-overrides '((file (styles basic partial-completion))))

(use-package! corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode))

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package! orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Use dabbrev with Corfu!
(use-package! dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; A few more useful configurations...
(use-package! emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; (use-package! platformio-mode
;;   :config
;;   (add-to-list 'company-backends 'company-irony))

(after! irony
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
(after! poetry
  (setq poetry-tracking-strategy 'projectile))

(after! cider
  (setq nrepl-use-ssh-fallback-for-remote-hosts t
        cider-clojure-cli-aliases ":add-libs"))

;; (use-package! geiser
;;   :config
;;   (setq geiser-active-implementations '(chicken))
;;   (setq geiser-chicken-binary "/usr/bin/chicken-csi"))

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
         " AND maildir:/INBOX")))

;; (use-package! helm-bibtex
;;   :config
;;   (setq bibtex-completion-pdf-field "File")
;;   (setq bibtex-completion-pdf-open-function 'find-file))

;;(use-package! magit
 ;; :config
  ;;(setq magit-revision-show-gravatars t))

;;(use-package! org-transclusion
;;  :defer
;;  :after org
;;  :init
;;  (map!
;;   :map global-map "<f12>" #'org-transclusion-add
;;   :leader
;;   :prefix "n"
;;   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

;; so ssh into guix machine has proper path (for example for ls)
(after! tramp
 (setq tramp-remote-path
       (append tramp-remote-path
                  '("~/.guix-profile/bin" "~/.guix-profile/sbin"
                    "/run/current-system/profile/bin"
                    "/run/current-system/profile/sbin"))))

(with-eval-after-load 'guix-repl
    ;; (setq guix-guile-program  "guile"
    (setq guix-guile-program  '("guix" "repl")
          guix-config-scheme-compiled-directory  nil
          guix-repl-use-latest  nil
          guix-repl-use-server  nil))

;; (defun guix-buffer-p (&optional buffer)
;;   (let ((buf-name (buffer-name (or buffer (current-buffer)))))
;;     (not (null (or (string-match "*Guix REPL*" buf-name)
;; 		   (string-match "*Guix Internal REPL*" buf-name))))))

;; (defun guix-geiser--set-project (&optional _impl _prompt)
;;   (when (and (eq 'guile geiser-impl--implementation)
;; 	     (null geiser-repl--project)
;; 	     (guix-buffer-p))
;;     (geiser-repl--set-this-buffer-project 'guix)))

;; (advice-add 'geiser-impl--set-buffer-implementation :after #'guix-geiser--set-project)
