;;; org-slide.el --- embedded slideshows in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) TODO: YEAR NAME

;; Author:   TODO: NAME <MAIL>
;; Keywords: TODO: lisp
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;; Homepage: TODO
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

;; TODO Put a description of the package here

;;; Code:

;;; ORG-SLIDE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org) ;; TODO should this go here or should the user be required to
               ;;      load org before loading this pkg

(defun org-dblock-write:org-slide (params)
  (let ((text      (plist-get params :content))
        (count     (plist-get params :count)) ;; TODO check for nil, don't break when missing
        (delimiter (plist-get params :delimiter))
        (slides '(0))
        new-count
        pos-of-block-in-buf
        end-of-block-in-buf)
    (unless delimiter (setq delimiter "#+SLIDE"))
    (setq new-count (1+ count))
    (setq delimiter (regexp-quote delimiter)) ; escape special regex chars
    (setq pos-of-block-in-buf (point))
    (setq text (substring text 0 -1)) ; remove last newline from text
    (save-excursion (insert text))

    (setq last-match (string-match delimiter text))
    (while (not (equal last-match nil)) ;; fill slides list with indices
      (setq slides (append slides (list last-match))) ;; TODO there must be a better way...
      (setq last-match (string-match delimiter text (+ (car (last slides)) (length delimiter)))))
    (setq slides (append slides (list (length text))))
    (setq end-of-block-in-buf (+ pos-of-block-in-buf (car (last slides))))

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

    (org-latex-preview)         ; preview latex in case they are part of the slide
    (org-display-inline-images) ; show images in case they are part of the slide
))

;;; CREATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-slide-insert-dblock ()
  "Create a org-slide dynamic block at point.
   TODO Let the block inherits its properties from a variable
       `org-slide-default-properties'."
  (interactive)
  (org-create-dblock (list :name "org-slide" :count 0 :delimiter "#+SLIDE")))

(add-to-list 'org-dynamic-block-alist '("slide" . org-slide-insert-dblock))

;;; EXPORT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-slide-export (text backend info)
  "Remove the slide delimiters from the html"
  (when (org-export-derived-backend-p backend 'html)
    (print text)
    ; TODO pull the delimiter argument out the plist for the dynamic block - but
    ; when looking at parse-tree: (dynamic-block (... :block-name "update" :arguments nil ..))
    (replace-regexp-in-string (regexp-quote "#+SLIDE") "" text)))

; TODO can cause an error because ox gets lazy-loaded
(add-to-list 'org-export-filter-dynamic-block-functions 'org-slide-export)

(provide 'org-slide)
;;; test.el ends here
