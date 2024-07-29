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

(defmacro asdf-message (format-string &rest args)
  (and format-string
       `(message
         (string-remove-suffix
          "\n"
          (eval-when-compile ,(concat "[asdf]: " format-string)))
         ,@args)))

(defmacro asdf-process-buffer (&optional no-erase)
  `(with-current-buffer (get-buffer-create asdf-process-buffer-name)
     ,(unless no-erase
        `(let ((inhibit-read-only t)) (erase-buffer)))
     (asdf-process-mode)
     (current-buffer)))

(cl-defmacro asdf-process-lines (cmd &rest args &key process-fn &allow-other-keys)
  "Call `process-lines' with asdf CMD on args.
If PROCESS-FN is non-nil, apply to each line of results (default trim ws)."
  (declare (debug t))
  (while (keywordp (car args))
    (setq args (cdr (cdr args))))
  `(mapcar ,(or process-fn ''string-trim)
           ,(if args `(funcall #'process-lines "asdf" ,cmd ,@args)
              `(process-lines "asdf" ,cmd))))

(cl-defmacro with-asdf-output (cmd plugin version &rest body
                                   &key error &allow-other-keys)
  "Call asdf CMD for PLUGIN VERSION."
  (declare (indent 3) (debug t))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(let* ((buff (asdf-process-buffer))
          (proc (start-process "asdf" buff "asdf" ,cmd ,plugin ,version)))
     (set-process-filter proc 'asdf-process-filter)
     (set-process-sentinel
      proc
      #'(lambda (p m)
          (asdf-message "%s" m)
          (if (not (zerop (process-exit-status p)))
              ,(if error `,error
                 `(asdf-message "%s %s failed" ,plugin ,cmd))
            ,@body)))))

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
         (asdf-process-lines ,(if all "plugin-list-all" "plugin-list"))))
     ((eq type 'version)
      (if (and all (null plugin))
          (user-error "list-all must be called with a plugin"))
      `(asdf-completing-read
         "Version: " (,@(if plugin '(nreverse) '(progn))
                      (asdf-process-lines
                       ,(if all "list-all" "list")
                       ,@(delq nil (if all `(:process-fn 'identity ,plugin)
                                     `(,plugin)))))))
     (t (user-error "%S unknown to `asdf-read'." type)))))

(defmacro asdf--read-plugin/version (&optional all)
  (let ((p (make-symbol "plugin")))
    `(let ((,p (asdf-read 'plugin ,all)))
       (list ,p (asdf-read 'version ,p ,all)))))

;; -------------------------------------------------------------------
;;; List mode

(defmacro asdf--list-name/version ()
  '(list (aref (tabulated-list-get-entry) 3) (tabulated-list-get-id)))

(defmacro asdf-list-buffer ()
  '(get-buffer-create asdf-buffer-name))

(provide 'asdf-macs)
;;; asdf-macs.el ends here
