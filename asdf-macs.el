;;; asdf-macs.el ---  -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/asdf
;; Package-Requires:
;; Created:  1 September 2018

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; TODO(7/29/24): why are these macros?
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(declare-function "asdf" asdf-completing-read)
(defvar asdf-buffer-name)
(defvar asdf-process-buffer-name)

(defsubst asdf-message (format-string &rest args)
  (and format-string
       (apply #'message
              (string-remove-suffix
               "\n" (concat "[asdf]: " format-string))
              args)))

(defmacro asdf-process-buffer (&optional no-erase)
  `(with-current-buffer (get-buffer-create asdf-process-buffer-name)
     ,(unless no-erase
        `(let ((inhibit-read-only t)) (erase-buffer)))
     (asdf-process-mode)
     (current-buffer)))

(cl-defmacro asdf-process-lines (cmd &rest args &key process-fn
                                     &allow-other-keys)
  "Call `process-lines' with asdf CMD on args.
If PROCESS-FN is non-nil, apply to each line of results (default trim ws)."
  (declare (debug t))
  (while (keywordp (car args))
    (setq args (cdr (cdr args))))
  `(delete "" (mapcar ,(or process-fn ''string-trim)
                      ,(if args `(funcall #'process-lines "asdf" ,cmd ,@args)
                         `(process-lines "asdf" ,cmd)))))

(cl-defmacro with-asdf-output (cmd plugin version &rest body
                                   &key error &allow-other-keys)
  "Call asdf CMD for PLUGIN VERSION."
  (declare (indent 3) (debug t))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (macroexp-let2* nil ((cmd cmd))
    `(let* ((buff (asdf-process-buffer))
            (args (append (if (listp ,cmd) ,cmd (list ,cmd))
                          (list ,plugin ,version)))
            (proc (apply #'start-process "asdf" buff "asdf" args)))
       (set-process-filter proc 'asdf-process-filter)
       (set-process-sentinel
        proc
        #'(lambda (p m)
            (asdf-message "%s" m)
            (if (not (zerop (process-exit-status p)))
                ,(if error `,error
                   `(asdf-message "%s: %S failed" ,plugin ,cmd))
              ,@body))))))

(defmacro asdf-read (type &optional plugin all)
  "Read asdf command of TYPE (listed below).
If ALL is non-nil use the all version of the asdf command.

`plugin' -- plugin-list/plugin-list-all
`version' -- list/list-all"
  (let ((type (eval type)))             ;assumes quoted type, eg 'plugin
    (cond
     ((eq type 'plugin)
      `(asdf-completing-read
         "Plugin: "
         (asdf-process-lines "plugin" "list" ,@(and all '("all")))))
     ((eq type 'version)
      (if (and all (null plugin))
          (user-error "list-all must be called with a plugin"))
      `(asdf-completing-read
         "Version: " (,@(if plugin '(nreverse) '(progn))
                      (asdf-process-lines
                       "list"
                       :process-fn ,(if all ''identity
                                      (lambda (s) (string-remove-prefix " *" s)))
                       ,@(and all '("all"))
                       ,plugin))))
     (t (user-error "%S unknown to `asdf-read'." type)))))


(provide 'asdf-macs)
;;; asdf-macs.el ends here
