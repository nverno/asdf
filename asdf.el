;;; asdf --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/asdf
;; Package-Requires: 
;; Created: 24 December 2016

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

;; [![Build Status](https://travis-ci.org/nverno/asdf.svg?branch=master)](https://travis-ci.org/nverno/asdf)

;;; Description:

;; asdf version manager

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar tabulated-list-format)
  (defvar tabulated-list-entries))
(declare-function tabulated-list-init-header "tabulated-list")
(declare-function tabulated-list-print "tabulated-list")
(declare-function tabulated-list-get-entry "tabulated-list")

(defgroup asdf nil "asdf")

(defface asdf-checkmark-face
  '((t (:background "grey10" :foreground "green")))
  "checkmark"
  :group 'asdf)

;; -------------------------------------------------------------------
;;; List

;; format list of available / installed versions for PLUGIN
(defsubst asdf--versions (plugin)
  (let ((all (process-lines "asdf" "list-all" plugin))
        (inst (ignore-errors (process-lines "asdf" "list" plugin))))
    (cl-loop for v in all
       collect (list v (vector v (if (cl-member v inst :test 'string=)
                                     (propertize "✓" 'face 'asdf-checkmark-face)
                                   "")
                               plugin)))))

;; remove ^M from output and scroll to bottom
(defun asdf-list--filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

;; revert buffer
(defun asdf-list-revert ()
  (interactive)
  (when (eq major-mode 'asdf-list-mode)
    (message "Reloading asdf-list...")
    (setq tabulated-list-entries (nreverse (asdf--versions asdf-list-plugin)))))

;;;###autoload
(defun asdf-list (plugin)
  "List asdf managed versions for PLUGIN."
  (interactive
   (list
    (ido-completing-read "Plugin: " (process-lines "asdf" "plugin-list"))))
  (let ((ver (asdf--versions plugin))
        (buff (get-buffer-create "*asdf*")))
    (with-current-buffer buff
      (setq tabulated-list-format `[(,plugin 20 nil)
                                    ("Installed" 5 nil)])
      (setq tabulated-list-entries (nreverse ver))
      (asdf-list-mode)
      (setq-local asdf-list-plugin plugin)
      (pop-to-buffer (current-buffer)))))

;; install VERSION of PLUGIN at point in `asdf-list-mode'
(defun asdf-list-install (plugin version)
  (interactive (list (aref (tabulated-list-get-entry) 2)
                     (tabulated-list-get-id)))
  (let ((proc (start-process "asdf" "*asdf-process*" "asdf"
                             "install" plugin version)))
    (set-process-filter proc 'asdf-list--filter)
    (set-process-sentinel
     proc
     (lambda (p m)
       (if (zerop (process-exit-status p))
           (with-current-buffer (process-buffer p)
             (view-mode)))
       (message "[asdf] %s: %s" (process-name p) (substring m 0 -1)))))
  (display-buffer "*asdf-process*"))

;;; Mode

(defvar asdf-list-menu
  '("asdf"
    ["Install" asdf-list-install t]
    ["Revert" asdf-list-revert t]))

(defvar asdf-list-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil asdf-list-menu)
    (define-key km (kbd "RET") 'asdf-list-install)
    (define-key km "g"         'asdf-list-revert)
    (define-key km "j"         'forward-line)
    (define-key km "k"         'previous-line)
    km))

(define-derived-mode asdf-list-mode tabulated-list-mode "asdf"
  "List of available asdf versions/plugings.\n
Commands: \n
\\{asdf-list-mode-map}"
  (tabulated-list-init-header)
  (tabulated-list-print))
        
(provide 'asdf)
;;; asdf.el ends here
