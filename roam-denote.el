;;; roam-denote.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Antonio Petrillo
;;
;; Author: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Maintainer: Antonio Petrillo <antonio.petrillo4@studenti.unina.it>
;; Created: May 20, 2025
;; Modified: May 20, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/nto/roam-denote
;; Package-Requires: ((emacs "28.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; The main problem here is that org-roam is focused around the `org-capture' system, which I should say is not that intuitive and pleasant to use.
;; To hack around it I store all the data need in global variable during the filename generation step.
;; Then I read again the data and insert into the new capture buffer following the frontmatter scheme, in the end I set all the variables to nil.
;;
;; I know, not the best solution but it works.
;;
;;; Code:


(require 'org-roam)
(require 'org-roam-protocol)
(require 'denote)

(defvar nto/org-roam-inherit-tags
  "here are stored tags that a new note should inherit from the current one"
  '())

(defun nto/org-roam-get-inherit-keywords ()
  "Let the user choose the keywords to pass from a node to the child and store into the global variable `nto/org-roam-inherit-tags'"
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
         (keywords (denote-extract-keywords-from-path filename))
         (inheriteds (delete-dups
                      (completing-read-multiple
                       (format-prompt "Select KEYWORDS to inherit" nil)
                       keywords))))
    (setq nto/org-roam-inherit-tags inheriteds)
    (funcall-interactively #'org-roam-node-insert)))

(defun nto/new-org-roam-filename (title &optional fixed-tags template-action)
  "Read all the data needed to configure a denote note and set the respective variables."
  (let* ((time (current-time))
         (id (denote-get-identifier time))
         (tags (delete-dups (append (denote-keywords-prompt) fixed-tags nto/org-roam-inherit-tags)))
         (template (pcase template-action
                     ('prompt (denote-template-prompt))
                     ('() nil)
                     (_ template-action))))
    (setq nto/org-roam-capture-id id
          nto/org-roam-capture-title title
          nto/org-roam-capture-tags (denote-keywords-sort tags)
          nto/org-roam-capture-date (format-time-string "[%F %a %R]" time)
          nto/org-roam-template template)
    (denote--keywords-add-to-history tags)
    (thread-first
      (denote-format-file-name "/" id tags title ".org" nil)
      (substring 1))))

(defun nto/new-org-roam-template ()
  "Use the global values to generate the frontmatter for the file and the reset all to nil."
  (let* ((filetags (if nto/org-roam-capture-tags
                       (concat ":" (mapconcat #'identity nto/org-roam-capture-tags ":") ":")))
         (front-matter (concat
                        ":PROPERTIES:\n"
                        ":ID:        " nto/org-roam-capture-id "\n"
                        ":END:\n"
                        "#+title:      " nto/org-roam-capture-title "\n"
                        "#+date:       " nto/org-roam-capture-date "\n"
                        "#+filetags:   " filetags "\n"
                        "#+identifier: " nto/org-roam-capture-id "\n\n"
                        (alist-get nto/org-roam-template denote-templates ""))))
    (setq nto/org-roam-capture-id nil
          nto/org-roam-capture-title nil
          nto/org-roam-capture-tags nil
          nto/org-roam-capture-date nil
          nto/org-roam-inherit-tags nil)
    front-matter))

;; just some experiment to integrate also the denote template

(setq denote-templates
      '((empty . "")
        (meta . "Meta note that talks about ")
        (todo . "* TODO this note will talk about ")
        (spark . "* TODO process ")))

(setq org-roam-capture-templates
      '(("n" "note" plain "%?"
         :target (file+head "%(nto/new-org-roam-filename \"${title}\")"
                            "%(nto/new-org-roam-template)")
         :unnarrowed t)
        ("m" "meta note" plain "%?"
         :target (file+head "%(nto/new-org-roam-filename \"${title}\" '(\"meta\") 'meta)"
                            "%(nto/new-org-roam-template)")
         :unnarrowed t)
        ("s" "spark" plain "%/"
         :target (file+head "%(nto/new-org-roam-filename \"${title}\" '(\"spark\") 'spark)"
                            "%(nto/new-org-roam-template)")
         :unnarrowed t)
        ("w" "web" plain "%/"
         :target (file+head "%(nto/new-org-roam-filename \"${title}\" '(\"spark\" \"fromweb\") 'spark)"
                            "%(nto/new-org-roam-template)")
         :unnarrowed t)))

(provide 'roam-denote)
;;; roam-denote.el ends here
