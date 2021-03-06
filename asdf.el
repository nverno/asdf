;;; asdf.el --- asdf version manager -*- lexical-binding: t; -*-

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

;; emacs interface to [asdf version manager](https://github.com/asdf-vm/asdf)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'asdf-macs)
  (defvar tabulated-list-format)
  (defvar tabulated-list-entries)
  (defvar asdf-list-plugin))

(declare-function tabulated-list-init-header "tabulated-list")
(declare-function tabulated-list-print "tabulated-list")
(declare-function tabulated-list-get-entry "tabulated-list")

(defgroup asdf nil
  "asdf package manager"
  :group 'external)

(defvar asdf-after-use-hook ()
  "Hook run after `asdf-use' sets current version.")

(defface asdf-checkmark-face
  '((t (:background "grey10" :foreground "green")))
  "checkmark"
  :group 'asdf)

(defalias 'asdf-completing-read 'ido-completing-read)

(defvar asdf-buffer-name "*asdf*")
(defvar asdf-process-buffer-name "*asdf-process*")

;; read version of PLUGIN from .tool-versions. Find first .tool-versions
;; file by default, or if GLOBAL look in user's home dir or use
;; GLOBAL-FILE if non-nil
(defun asdf--read-version-file (plugin &optional global global-file)
  (let ((conf
         (if (not global)
             (locate-dominating-file
              (or buffer-file-name default-directory) ".tool-versions")
           (or global-file (expand-file-name ".tool-versions" (getenv "HOME"))))))
    (when conf
      (with-temp-buffer
        (insert-file-contents (expand-file-name ".tool-versions" conf))
        (when (re-search-forward
               (concat "^" plugin " +\\([^ \t\n\r]+\\)") nil 'move)
          (match-string-no-properties 1))))))

;; remove ^M from output, scroll to bottom, format a bit
(defun asdf-process-filter (proc string)
  (unless (string= string "")
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      ;; if curl is making progress bars, show on just one line
      (if (ignore-errors
            (beginning-of-line)
            (looking-at-p "[ \t]*#*\\([0-9\.%]+$\\)"))
          (progn (ignore-errors (backward-char))
                 (delete-region (point) (point-max)))
        (forward-line))
      (insert (replace-regexp-in-string "[\r\n]+" "\n" string)))))

;; -------------------------------------------------------------------
;;; General commands

;;;###autoload
(defun asdf-install (plugin version &optional error success)
  (interactive (asdf--read-plugin/version))
  (with-asdf-output "install" plugin version
    :error (if error (funcall error))
    (if success (funcall success))))

;;;###autoload
(defun asdf-use (plugin version &optional local)
  (interactive (asdf--read-plugin/version))
  (let ((res (call-process "asdf" nil nil nil
                           (if local "local" "global") plugin version)))
    (if (not (zerop res))
        (asdf-message "%s" res)
      (asdf-message "using %s %s" plugin version)
      (run-hooks 'asdf-after-use-hook))))

;;;###autoload
(defun asdf-current-version (plugin)
  "Return current version for PLUGIN."
  (ignore-errors
    (car (split-string (car (process-lines "asdf" "current" plugin)) "[ (]"))))

;;;###autoload
(defun asdf-where (plugin)
  "Full path to current PLUGIN version root directory."
  (ignore-errors (car (process-lines "asdf" "where" plugin))))

;; -------------------------------------------------------------------
;;; List

;; format list of available / installed versions for PLUGIN
(defun asdf--versions (plugin)
  (let ((all (process-lines "asdf" "list-all" plugin))
        (inst (ignore-errors
                (mapcar #'string-trim (process-lines "asdf" "list" plugin))))
        (current (asdf-current-version plugin)))
    (cl-loop for v in all
       collect (list v (vector v (if (cl-member v inst :test 'string=)
                                     (propertize "✓" 'face 'asdf-checkmark-face)
                                   "")
                               (if (string= v current)
                                   (propertize "✓" 'face 'font-lock-warning-face)
                                 "")
                               plugin)))))

;;;###autoload
(defun asdf-list (plugin)
  "List asdf managed versions for PLUGIN."
  (interactive (list (asdf-read 'plugin)))
  (let ((ver (asdf--versions plugin)))
    (with-current-buffer (asdf-list-buffer)
      (setq tabulated-list-format `[(,plugin 20 t)
                                    ("Installed" 15 nil)
                                    ("Current" 10 nil)])
      (setq tabulated-list-entries (nreverse ver))
      (asdf-list-mode)
      (setq-local asdf-list-plugin plugin)
      (pop-to-buffer (current-buffer)))))

;; -------------------------------------------------------------------
;;; Interactive list mode functions

(defun asdf-list-revert ()
  "Revert asdf-list mode buffer."
  (interactive)
  (when (eq major-mode 'asdf-list-mode)
    (asdf-message "Reloading asdf-list...")
    (setq tabulated-list-entries (nreverse (asdf--versions asdf-list-plugin)))
    (revert-buffer)))

(defun asdf-list-install (plugin version)
  "Install PLUGIN VERSION at point."
  (interactive (asdf--list-name/version))
  (and (y-or-n-p (format "Install %s %s?" plugin version))
       (with-asdf-output "install" plugin version
         (with-current-buffer (asdf-list-buffer)
           (asdf-list-revert))
         (with-current-buffer (asdf-process-buffer)
           (view-mode)))
       (display-buffer asdf-process-buffer-name)))

;; switch asdf current version to version at point in `asdf-list-mode'
(defun asdf-list-use (plugin version &optional local)
  (interactive (asdf--list-name/version))
  (with-asdf-output (if local "local" "global") plugin version
    (with-current-buffer (asdf-list-buffer) (asdf-list-revert))
    (asdf-message "%s %s using %s" (if local "local" "global") plugin version))
  (run-hooks 'asdf-after-use-hook))

(defun asdf-list-use-local (plugin version)
  (interactive (asdf--list-name/version))
  (asdf-list-use plugin version 'local))

(defun asdf-list-uninstall (plugin version)
  (interactive (asdf--list-name/version))
  (and (y-or-n-p (format "Uninstall %s %s?" plugin version))
       (with-asdf-output "uninstall" plugin version
         (with-current-buffer (asdf-list-buffer) (asdf-list-revert))
         (asdf-message "uninstalled %s %s" plugin version))))

(defun asdf-list-where (plugin version)
  (interactive (asdf--list-name/version))
  (with-asdf-output "where" plugin version
    (asdf-message "%s not installed" (tabulated-list-get-id))
    (with-current-buffer (asdf-process-buffer 'no-erase)
      (asdf-message "%s" (buffer-string)))))

;; -------------------------------------------------------------------
;;; Modes

(defvar asdf-list-menu
  '("asdf"
    ["Install" asdf-list-install t]
    ["Set global version" asdf-list-use t]
    ["Set local version" asdf-list-use-local t]
    ["Uninstall" asdf-list-uninstall t]
    ["Installation path" asdf-list-where t]
    ["Revert" asdf-list-revert t]))

(defvar asdf-list-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil asdf-list-menu)
    (define-key km "j"         'forward-line)
    (define-key km "k"         'previous-line)
    (define-key km (kbd "RET") 'asdf-list-install)
    (define-key km "i"         'asdf-list-install)
    (define-key km "r"         'asdf-list-revert)
    (define-key km "v"         'asdf-list-use)
    (define-key km "l"         'asdf-list-use-local)
    (define-key km "u"         'asdf-list-uninstall)
    (define-key km "w"         'asdf-list-where)
    km))

(define-derived-mode asdf-list-mode tabulated-list-mode "asdf"
  "List of available asdf versions/plugings.\n
Commands: \n
\\{asdf-list-mode-map}"
  (tabulated-list-init-header)
  (tabulated-list-print))
        
(define-derived-mode asdf-process-mode nil "asdf-process"
  nil
  (setq mode-line-process '(":%s")))

(provide 'asdf)
;;; asdf.el ends here
