;;; org-slide.el --- embedded slideshows in org-mode -*- lexical-binding: t; -*-

;; Author: dandersch
;; URL: https://github.com/dandersch/org-slide
;; Package-Requires: ((emacs "28.1") (org "9.5")) TODO try to support older versions
;; Keywords: org, slideshow
;; Version:  0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO description of the package

;;; Code:

(require 'org)
(defvar org-slide-alist '() "")
(make-variable-buffer-local 'org-slide-alist)
(defun org-dblock-write:org-slide (params)
  "Show the next slide in an org-slide dynamic block.
PARAMS contains the text of the block and everything else passed by the user."
  (let* ((text      (substring (plist-get params :content) 0 -1)) ; remove last newline from text
         ; NOTE: we could allow custom delimiters here
         ;(delimiter (or (plist-get params :delimiter) "# SLIDE"))
         (delimiter "# SLIDE")
         ;(indent (plist-get params :indentation-column)) ; TODO account for indentation
         (slides '(0))
         (id     (plist-get params :id))
         (pos-of-block-in-buf (point))
         last-match
         (end-of-block-in-buf (+ pos-of-block-in-buf (length text))))
    (setq delimiter (regexp-quote delimiter)) ; escape special regex chars just in case
    (save-excursion (insert text))

    (setq count (alist-get id org-slide-alist -1 nil 'string-equal))
    (when (eq count -1) (progn (push (cons id "0") org-slide-alist) (setq count 0)))
    (setf new-count (1+ count))

    (setq last-match (string-match delimiter text))
    (while (not (equal last-match nil)) ;; fill slides list with indices
      (setq slides (append slides (list last-match))) ;; TODO is there a better way?
      (setq last-match (string-match delimiter text (+ (car (last slides)) (length delimiter)))))
    (setq slides (append slides (list end-of-block-in-buf)))

    (when (>= new-count (length slides)) (setq new-count 0)) ; wraparound

    ; TODO rewrite
    (outline-flag-region (- pos-of-block-in-buf 1) end-of-block-in-buf t)
    (let ((hide-from (+ (+ pos-of-block-in-buf (nth (- new-count 1) slides)) (- (length delimiter) 1)))
          (hide-to   (+ pos-of-block-in-buf (nth new-count slides))))
      (when (equal new-count 1) (setq hide-from (- pos-of-block-in-buf 1))) ; special case
      (if (equal new-count 0)
        (outline-flag-region (- pos-of-block-in-buf 2) end-of-block-in-buf nil)
        (outline-flag-region hide-from hide-to nil)))

    (setf (alist-get id org-slide-alist -1 nil 'string-equal) new-count)

    ; show latex and images in case they were part of the slide
    (not-modified)
    (org-latex-preview)
    (org-display-inline-images)))

(defun org-slide-insert-dblock ()
  "Create an org-slide dynamic block at point."
  (interactive)
  (org-create-dblock (list :name "org-slide" :id "slideshow-name"))
  (save-excursion
    (forward-line)
    (insert "1st slide...\n# SLIDE\n2nd slide...\n# SLIDE\n3rd slide...")))

(add-to-list 'org-dynamic-block-alist '("slide" . org-slide-insert-dblock))

; Each filter is called with three arguments: the transcoded data,
; as a string, the back-end, as a symbol, and the communication
; channel, as a plist.  It must return a string or nil.
; TODO support exporting org-slide blocks
(require 'ox)
(defun org-slide-export (text backend info)
  "TEXT BACKEND INFO."
  (when (org-export-derived-backend-p backend 'html) ()
    (concat
     "<style>"
     "div[id^=\"slide-id-\"] { /* visibility: hidden; */ display: none; }"
     ":target { display: inline-block !important; }"
     "</style>"
     "<a class=\"org-slide\" id=\"slide-start\" href=\"#slide-id-1\">Start</a> <br>"
     "<div class=\"org-slide-container\">"
         "<div class=\"org-slide\" id=\"slide-id-1\">"
     (string-replace "#+SLIDE" "</div> <div class=\"org-slide\" id=\"slide-id-2\"> <a class=\"org-slide\" id=\"slide-prev\" href=\"#slide-id-1\"><</a><a class=\"org-slide\" id=\"slide-next\" href=\"#slide-id-3\">></a> <br>" text)
     "</div></div>"
     )
;    (concat
;      "<style>"
;      "div[id^=\"slide-id-\"] { /* visibility: hidden; */ display: none; }"
;      ":target { display: inline-block !important; }"
;      "</style>"
;      "<a class=\"org-slide\" id=\"slide-start\" href=\"#slide-id-1\">Start</a> <br>"
;      "<div class=\"org-slide-container\">"
;            	  "<div class=\"org-slide\" id=\"slide-id-1\">"
;                "<a class=\"org-slide\" id=\"slide-prev\" href=\"#slide-id-3\"><</a>"
;                "<a class=\"org-slide\" id=\"slide-next\" href=\"#slide-id-2\">></a> <br>"
;            		"<p>First Slide</p>"
;            	  "</div>"
;            	  "<div class=\"org-slide\" id=\"slide-id-2\">"
;                "<a class=\"org-slide\" id=\"slide-prev\" href=\"#slide-id-1\"><</a>"
;                "<a class=\"org-slide\" id=\"slide-next\" href=\"#slide-id-3\">></a> <br>"
;            		"<h1>Second</h1>"
;            	  "</div>"
;            	  "<div class=\"org-slide\" id=\"slide-id-3\">"
;                "<a class=\"org-slide\" id=\"slide-prev\" href=\"#slide-id-2\"><</a>"
;                "<a class=\"org-slide\" id=\"slide-next\" href=\"#slide-id-1\">></a> <br>"
;            		"<p>Last</p>"
;            	  "</div>"
;      "</div>"
;      )
    )
  )

(add-to-list 'org-export-filter-dynamic-block-functions 'org-slide-export)

(provide 'org-slide)
;;; org-slide.el ends here
