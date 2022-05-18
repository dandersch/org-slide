;;; org-slide.el --- embedded slideshows in org-mode -*- lexical-binding: t; -*-

;; Author: dandersch
;; URL: TODO
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
(defun org-dblock-write:org-slide (params)
  "Show the next slide in an org-slide dynamic block.
PARAMS contains the text of the block and everything else passed by the user."
  (let* ((text      (substring (plist-get params :content) 0 -1)) ; remove last newline from text
         (count     (plist-get params :count)) ; TODO check for nil, don't break when missing
         (delimiter (or (plist-get params :delimiter) "#+SLIDE"))
         ;(indent (plist-get params :indentation-column)) ; TODO account for indentation
         (slides '(0))
         (new-count (1+ count))
         (pos-of-block-in-buf (point))
         last-match
         (end-of-block-in-buf (+ pos-of-block-in-buf (length text))))
    (setq delimiter (regexp-quote delimiter)) ; escape special regex chars
    (save-excursion (insert text))

    (setq last-match (string-match delimiter text))
    (while (not (equal last-match nil)) ;; fill slides list with indices
      (setq slides (append slides (list last-match))) ;; TODO is there a better way?
      (setq last-match (string-match delimiter text (+ (car (last slides)) (length delimiter)))))
    (setq slides (append slides (list end-of-block-in-buf)))

    (when (>= new-count (length slides)) (setq new-count 0)) ; wraparound

    (save-excursion ; return point to where it was after this call
      (let ((curr-count (concat ":count " (number-to-string count)))
            (next-count (concat ":count " (number-to-string new-count))))
        (forward-line -1)            ; go to #+BEGIN:... line
        (org-hide-block-toggle 'off) ; make block fully visible incase it's hidden
        (replace-string-in-region curr-count next-count nil pos-of-block-in-buf)))

    ; TODO rewrite
    (outline-flag-region (- pos-of-block-in-buf 1) end-of-block-in-buf t)
    (let ((hide-from (+ (+ pos-of-block-in-buf (nth (- new-count 1) slides)) (- (length delimiter) 1)))
          (hide-to   (+ pos-of-block-in-buf (nth new-count slides))))
      (when (equal new-count 1) (setq hide-from (- pos-of-block-in-buf 1))) ; special case
      (if (equal new-count 0)
        (outline-flag-region (- pos-of-block-in-buf 2) end-of-block-in-buf nil)
        (outline-flag-region hide-from hide-to nil)))

    ; show latex and images in case they were part of the slide
    (org-latex-preview)
    (org-display-inline-images)))

(defun org-slide-insert-dblock ()
  "Create an org-slide dynamic block at point."
  (interactive)
  (org-create-dblock (list :name "org-slide" :count 0 :delimiter "#+SLIDE"))
  (save-excursion
    (forward-line)
    (insert "1st slide...\n#+SLIDE\n2nd slide...\n#+SLIDE\n3rd slide...")))

(add-to-list 'org-dynamic-block-alist '("slide" . org-slide-insert-dblock))

; TODO support exporting org-slide blocks
;(require 'ox)
;(defun org-slide-export (text backend info)
;  "Remove the slide delimiters from the html."
;  (when (org-export-derived-backend-p backend 'html)
;    (print info)
;    ; TODO pull the delimiter argument out the plist for
;    ;      the dynamic block if possible
;    (replace-regexp-in-string (regexp-quote "#+SLIDE") "" text)))
;
;(add-to-list 'org-export-filter-dynamic-block-functions 'org-slide-export)

(provide 'org-slide)
;;; org-slide.el ends here
