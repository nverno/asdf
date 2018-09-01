;;; asdf-macros ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

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
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar asdf-buffer-name)
  (defvar asdf-process-buffer-name))

(defmacro asdf-message (format-string &rest args)
  (and format-string
       `(message (eval-when-compile ,(concat "[asdf]: " format-string)) ,@args)))

(defmacro asdf-list-buffer () `(get-buffer-create asdf-buffer-name))

(defmacro asdf-process-buffer (&optional no-erase)
  `(progn (with-current-buffer (get-buffer-create asdf-process-buffer-name)
            ,(unless no-erase
               `(let ((inhibit-read-only t)) (erase-buffer)))
            (asdf-process-mode)
            (current-buffer))))

;; call asdf CMD PLUGIN VERSION and do ERROR and BODY with process
;; output
(defmacro with-asdf-output (cmd &optional error &rest body)
  (declare (indent 2))
  `(let* ((buff (asdf-process-buffer))
          (proc (start-process "asdf" buff "asdf" ,cmd plugin version)))
     (set-process-filter proc 'asdf-list--filter)
     (set-process-sentinel
      proc
      #'(lambda (p m)
          (asdf-message "%s" (substring m 0 -1))
          (if (not (zerop (process-exit-status p)))
              ,(if error `,error
                 `(asdf-message "%s %s failed" plugin ,cmd))
            ,@body)))))

(defmacro asdf--name/version ()
  `(interactive
    (list (aref (tabulated-list-get-entry) 3)
          (tabulated-list-get-id))))

(defmacro asdf--read-plugin ()
  `(ido-completing-read
    "Plugin: " (nreverse (process-lines "asdf" "plugin-list"))))

(defmacro asdf--read-version (plugin &optional all)
  `(ido-completing-read
    "Version: " (nreverse (process-lines
                           "asdf" ,(if all "list-all" "list") ,plugin))))

(defmacro asdf--read-plugin/version ()
  `(let ((plugin (asdf--read-plugin)))
     (list plugin (asdf--read-version plugin))))

(provide 'asdf-macros)
;;; asdf-macros.el ends here
