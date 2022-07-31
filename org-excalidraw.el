;;; org-excalidraw.el --- Tools for working with excalidraw drawings -*- lexical-binding: t; -*-
;; Copyright (C) 2022 David Wilson

;; Author:  David Wilson <wdavew@gmail.com>
;; URL: https://github.com/wdavew/org-excalidraw
;; Created: 2022
;; Version: 0.1.0
;; Keywords: convenience, outlines
;; Package-Requires: ((org "9.3") (emacs "26.1") (f "0.20.0"))

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;; org-excalidraw.el is a package to for embedding excalidraw drawings into Emacs.
;;; it adds an org-mode link type for excalidraw files to support inline display
;;; and opening the diagrams from Emacs for editing.

;;; Code:
(require 'cl-lib)
(require 'filenotify)
(require 'f)
(require 'org-id)
(require 'ol)

(defun org-excalidraw-default-base ()
  "Get default JSON template used for new excalidraw files."
  "{
    \"type\": \"excalidraw\",
    \"version\": 2,
    \"source\": \"https://excalidraw.com\",
    \"elements\": [],
    \"appState\": {
      \"gridSize\": null,
      \"viewBackgroundColor\": \"#ffffff\"
    },
    \"files\": {}
  }
")

(defgroup org-excalidraw nil
  "Customization options for org-excalidraw."
  :group 'org
  :prefix "org-excalidraw-")

(defcustom org-excalidraw-directory "~/org-excalidraw"
  "Directory to store excalidraw files."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-base
  (org-excalidraw-default-base)
  "JSON string representing base excalidraw template for new files."
  :type 'string
  :group 'org-excalidraw)

(defun org-excalidraw-validate-excalidraw-file (path)
  "Validate the excalidraw file at PATH is usable."
  (unless
   (string-suffix-p ".excalidraw" path)
   (error "Excalidraw file must have .excalidraw extension")))

(defun org-excalidraw-open-file (path)
  "Open .excalidraw file located at PATH."
  (org-excalidraw-validate-excalidraw-file path)
  (shell-command (org-excalidraw-shell-cmd-open path system-type)))

(defun org-excalidraw-new-file (path)
  "Create and open a new excalidraw file at PATH."
  (org-excalidraw-validate-excalidraw-file path)
  (f-write org-excalidraw-base 'utf-8 path)
  (org-excalidraw-open-file path))

(defun org-excalidraw-handle-file-change (event)
  "Handle file update EVENT to convert files to svg."
  (when
      (string-equal (cadr event)  "renamed")
  (let ((filename (cadddr event)))
    (when (string-suffix-p ".excalidraw" filename)
    (shell-command (org-excalidraw-shell-cmd-to-svg filename))))))

(defun org-excalidraw-shell-cmd-to-svg (path)
  "Construct shell cmd for converting excalidraw file with PATH to svg."
  (concat "excalidraw_export --rename_fonts=true " (format "\"%s\"" path)))

(defun org-excalidraw-shell-cmd-open (path os-type)
  "Construct shell cmd to open excalidraw file with PATH for OS-TYPE."
   (if (eq os-type 'darwin)
       (concat "open " (shell-quote-argument path))
     (concat "xdg-open " (shell-quote-argument path))))

(defun org-excalidraw-to-svg (path)
  "Convert excalidraw at PATH to svg."
  (org-excalidraw-validate-excalidraw-file path)
  (shell-command (org-excalidraw-shell-cmd-open path system-type)))

(defun org-excalidraw-open-svg (path)
  "Open an the excalidraw file at linked to svg at PATH."
  (let ((excal-file-path (string-remove-suffix ".svg" path)))
    (org-excalidraw-open-file excal-file-path)))

(defun org-excalidraw-get-unique-name ()
  "Return a unique string for a new excalidraw filename."
  (format "%s.excalidraw" (org-id-uuid)))

(defun org-excalidraw-format-link (path)
  "Format a link to excalidraw file at PATH."
  (org-excalidraw-validate-excalidraw-file path)
  (format "[[excalidraw:%s.svg]]" path))

(defun org-excalidraw-check-dir (dir)
  "Check that org-excalidraw directory at DIR exists."
  (unless (f-dir? dir)
    (error "Excalidraw directory %s does not exist" dir)))

;;;###autoload
(defun org-excalidraw-create-drawing ()
  "Create an excalidraw drawing and insert an 'org-mode' link to it at Point."
  (interactive)
  (let* ((filename (org-excalidraw-get-unique-name))
         (path (f-join org-excalidraw-directory filename)))
    (insert (org-excalidraw-format-link path))
  (org-excalidraw-new-file path)))

;;;###autoload
(defun org-excalidraw-initialize ()
  "Setup excalidraw.el. Call this after 'org-mode initialization."
  (interactive)
  (org-excalidraw-check-dir org-excalidraw-directory)
        (file-notify-add-watch org-excalidraw-directory '(change) 'org-excalidraw-handle-file-change)
        (org-link-set-parameters "excalidraw"
                                :follow 'org-excalidraw-open-svg
                                :image-data-fun '+org-image-file-data-fn))

(provide 'org-excalidraw)
;;; org-excalidraw.el ends here
