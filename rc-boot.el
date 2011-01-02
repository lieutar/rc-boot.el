;;; rc-boot.el --- 

;; Copyright (C) 2010  lieutar

;; Author: lieutar <lieutar@1dk.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(defvar rc-directory    "~/.emacs.d/rc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rc-boot-current-loading-file ())
(defvar rc-boot-error-buffer nil)

(defmacro rc-boot-with-error-buffer (&rest form)
  `(progn
     (unless (buffer-live-p rc-boot-error-buffer)
       (setq rc-boot-error-buffer
             (get-buffer-create "*rc-boot-errors*")))
     (save-excursion
       (set-buffer rc-boot-error-buffer)
       ,@form)))

(defun rc-boot-error-message (form &rest vals)
  (apply 'message form vals)
  (rc-boot-with-error-buffer
   (insert (apply 'format form vals) "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rc-emacsen
  (mapconcat
   'identity
   (list 
    (cond ((featurep 'meadow) "meadow")
          (t "fsf"))
    (cdr (or (assq system-type
                   '((windows-nt . "nt")
                     (cygwin     . "cygwin")))
             (cons nil "unicom")))
    (number-to-string emacs-major-version))
   "-"))

(defun rc-emacsen-match (emacsen-spec)
  (let ((regex
         (format "\\`%s\\'"
                 (mapconcat
                  (lambda (spec)
                    (mapconcat
                     'regexp-quote
                     (split-string spec "@")
                     ".*?"))
                  (split-string emacsen-spec "\\.")
                  "\\|"))))
    (and (string-match regex rc-emacsen) t)))


(defmacro rc-emacsen-case (&rest forms)
  `(cond ,@(mapcar
            (lambda (form)
              (if (eq t (car form))
                  `(t ,@(cdr form))
                `((rc-emacsen-match ,(symbol-name (car form)))
                  ,@(cdr form))))
            forms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rc-boot-loaded-file-alist    ())

(defvar rc-provided
  '())
(defvar rc-load-path
  '("lisp"))

(defsubst rc-load:time< (a b)
  (or (< (car a) (car b))
      (and (= (car a)
              (car b))
           (< (car (cdr a))
              (car (cdr b))))))

(defsubst rc-load:is-file-new-p (file)
  (let ((slot (assoc file rc-boot-loaded-file-alist)))
    (if slot
        (rc-load:time< 
         (cdr slot)
         (nth 5 (file-attributes file)))
      t)))

(defsubst rc-load:do-file (file)
  (if (rc-load:is-file-new-p file)
      (condition-case e 
          (let ((rc-boot-current-loading-file file))
            (load-file file)
            (setq rc-boot-loaded-file-alist
                  (cons (cons file (current-time))
                        rc-boot-loaded-file-alist)))
        (error  (rc-boot-error-message
                 "%s"
                 (format "%S\n... caught when loading %s\n\n" e file))
                ))))

(defun rc-load:loadable-p (file)
  (when
   (string-match "\\(\\(\\.[^./\\\\]+\\)*\\)\\.el\\'" file)
   (or (not (match-string 2 file))
       (rc-emacsen-match (substring (match-string 1 file) 1)))))


(defun rc-load:locate-library (lib)
  (let ((result ())
        (found  nil)
        (rex-lib (format "^%s\\(\\.\\([^\\.]+\\)\\)?\\.el\\'"
                         (regexp-quote lib)))
        (rc-lp
         (apply 'append
                (mapcar (lambda (subdir)
                          (let ((dir (expand-file-name subdir rc-directory)))
                            (when (and (file-readable-p dir)
                                       (file-directory-p dir))
                              (list dir))))
                        rc-load-path))))
    (dolist (dir rc-lp)
      (dolist (file (directory-files dir))
        (when (string-match rex-lib file)
          (setq found t)
          (when (or (null (match-beginning 2))
                    (rc-emacsen-match (match-string 2 file)))
            (setq result (cons (expand-file-name file dir) result))))))
    (unless found (error "Could not find library: %s" lib))
    (reverse result)))

;;(rc-load:locate-library "hoge")
;;(rc-load:locate-library "frame")
;;(rc-load:locate-library "font")

(defun rc-load (lib)
  (dolist (file (rc-load:locate-library lib))
    (rc-load:do-file file)))

(defun rc-require (feature)
  (unless (member feature rc-provided)
    (dolist (file (rc-load:locate-library (symbol-name feature)))
      (rc-load:do-file file))
    (setq rc-provided (cons feature rc-provided))
    feature))

(defun rc-load-directory:file-list (rc-dir)
  (let ((dir (expand-file-name rc-dir rc-directory )))
    (if (and (file-readable-p dir)
             (file-directory-p dir))
        (apply
         'append
         (mapcar
          (lambda (file)
            (let ((boot-file (expand-file-name file dir)))
              (if (rc-load:loadable-p boot-file)
                  (list boot-file))
              ))
          (directory-files dir)))
      )))
;;(rc-load-directory:file-list "ext")


(defun rc-load-directory (dir)
  (dolist (file (rc-load-directory:file-list dir))
    (rc-load:do-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rc-subdirs-list
  '(
    "first"    ;; Definitions of symbols for configuration files.
    "base"     ;; Basic configurations for your emacs environment.
    "ext"      ;; Loading extensions and setup them.
    "funcs"    ;; Definitions of commands and functions for your operation.
    "projects" ;; Definitions for your projects.
    "private"  ;; Definitions for your personal informations.
    "last"     ;; Last actions of configuration.
    ))

(defun rc-boot:file-list ()
  (apply
   'append
   (mapcar 'rc-load-directory:file-list
    rc-subdirs-list)))


(defun rc-boot ()
  (interactive)
  (dolist (file (rc-boot:file-list))
    (rc-load:do-file file)))



(provide 'rc-boot)
;;; rc-boot.el ends here
