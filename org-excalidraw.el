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

(defun org-excalidraw-default-base ()
  "Return default base excalidraw.json. This template is used for all new excalidraw files."
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

(defun org-excalidraw-open-file (path)
  "Open .excalidraw file located at PATH."
  (cl-assert (string-suffix-p ".excalidraw" path))
  (shell-command
   (if (eq system-type 'darwin)
       (concat "open " (shell-quote-argument path))
     (concat "xdg-open " (shell-quote-argument path)))))

(defun org-excalidraw-new-file (path)
  "Create and open a new excalidraw file at PATH."
  (cl-assert (string-suffix-p ".excalidraw" path))
  (f-write org-excalidraw-base 'utf-8 path)
  (org-excalidraw-open-file path))

(defun org-excalidraw-handle-file-change (event)
  "Handle file update EVENT to convert files to svg."
  (when
      (string-equal (cadr event)  "renamed")
  (let ((filename (cadddr event)))
    (when (string-suffix-p ".excalidraw" filename)
    (org-excalidraw-to-svg filename)))))

(defun org-excalidraw-to-svg (filename)
  "Convert excalidraw with FILENAME to svg."
  (cl-assert (string-suffix-p ".excalidraw" filename))
  (shell-command (concat "excalidraw_export --rename_fonts=true " (format "\"%s\"" filename))))

(defun org-excalidraw-open-svg (path)
  "Open an the excalidraw file at PATH for a given SVG."
  (let ((excal-file-path (string-remove-suffix ".svg" path)))
    (message "opening %S" excal-file-path)
    (org-excalidraw-open-file excal-file-path)))

(defun org-excalidraw-get-unique-name ()
  "Format current time to disambiguate files."
  (format-time-string "%Y-%m-%d-%H-%M-%S" (current-time)))

;;;###autoload
(defun org-excalidraw-create-drawing ()
  "Create an excalidraw drawing and insert an 'org-mode' link to it at Point."
  (interactive)
  (let* ((id (org-excalidraw-get-unique-name))
         (path (f-join org-excalidraw-directory (format "%s.excalidraw" id))))
    (insert "[[" "excalidraw:" path ".svg" "]]")
  (org-excalidraw-new-file path)))

;;;###autoload
(defun org-excalidraw-initialize ()
  "Setup excalidraw.el. Call this after 'org-mode initialization."
  (interactive)
        (file-notify-add-watch org-excalidraw-directory '(change) 'org-excalidraw-handle-file-change)
        (org-link-set-parameters "excalidraw"
                                :follow 'org-excalidraw-open-svg
                                :image-data-fun '+org-image-file-data-fn))

(provide 'org-excalidraw)
;;; org-excalidraw.el ends here
