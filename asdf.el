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
  (require 'asdf-macs))
(require 'tabulated-list)

(defgroup asdf nil
  "Asdf package manager."
  :group 'external)

(defcustom asdf-list-all-default nil
  "When non-nil list all available plugin versions by default.
Otherwise, default to listing installed versions only."
  :group 'asdf
  :type 'boolean
  :safe 'booleanp)

(defvar asdf-after-use-hook ()
  "Hook run after `asdf-use' sets current version.")

(defface asdf-checkmark-face
  '((t (:background "grey10" :foreground "green")))
  "Checkmark."
  :group 'asdf)

(defalias 'asdf-completing-read 'completing-read)

(defvar asdf-buffer-name "*asdf*")
(defvar asdf-process-buffer-name "*asdf-process*")

(defun asdf--read-version-file (plugin &optional global global-file)
  "Read version of PLUGIN from .tool-versions.
Find first .tool-versions file from current directory up, or if GLOBAL look
in user's home dir or use GLOBAL-FILE if non-nil."
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

(defun asdf-process-filter (proc string)
  "Remove ^M from PROC output STRING, scroll to bottom, and format a bit."
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
  "Install PLUGIN VERSION using asdf.
Call ERROR and SUCCESS if non-nil according to process status."
  (interactive (asdf--read-plugin/version))
  (with-asdf-output "install" plugin version
    :error (if error (funcall error))
    (if success (funcall success))))

;;;###autoload
(defun asdf-use (plugin version &optional local)
  "Tell asdf to Use PLUGIN VERSION.
If LOCAL, use locally."
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
    (cadr (split-string (car (process-lines "asdf" "current" plugin)) "[ (]+"))))

;;;###autoload
(defun asdf-where (plugin)
  "Full path to current PLUGIN version root directory."
  (ignore-errors (car (process-lines "asdf" "where" plugin))))

;; -------------------------------------------------------------------
;;; List

(defvar-local asdf--list-plugin nil)

(defvar-local asdf--list-show-all asdf-list-all-default)

(defvar-local asdf--list-available-versions nil)

(defun asdf--installed-versions (plugin)
  "Get current and installed versions of PLUGIN."
  (let* ((current)
         (installed
          (ignore-errors
            (cl-loop for version in (mapcar #'string-trim
                                            (process-lines "asdf" "list" plugin))
                     when (string-prefix-p "*" version)
                     do (setq version (substring version 1)
                              current version)
                     collect version))))
    (cons current installed)))

(defun asdf--versions (plugin &optional available)
  "Format list of installed versions for PLUGIN.
If AVAILABLE, include available versions."
  (cl-destructuring-bind (current . installed) (asdf--installed-versions plugin)
    (cl-loop for v in (if (not available) installed
                        (or asdf--list-available-versions
                            (setq asdf--list-available-versions
                                  (process-lines "asdf" "list-all" plugin))))
             collect (list v (vector v (if (cl-member v installed :test 'string=)
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
  (with-current-buffer (asdf-list-buffer)
    (setq asdf--list-available-versions nil)
    (let ((ver (asdf--versions plugin asdf--list-show-all))
          (all asdf--list-available-versions))
      (setq tabulated-list-format `[(,plugin 20 t)
                                    ("Installed" 15 nil)
                                    ("Current" 10 nil)])
      (setq tabulated-list-entries (nreverse ver))
      (asdf-list-mode)
      (setq-local asdf--list-plugin plugin)
      (setq-local asdf--list-available-versions all)
      (pop-to-buffer (current-buffer)))))

;; -------------------------------------------------------------------
;;; Interactive list mode functions

(defun asdf-list-revert (&rest _)
  "Revert `asdf-list' mode buffer."
  (interactive)
  (when (eq major-mode 'asdf-list-mode)
    (asdf-message "Reloading asdf-list...")
    (setq tabulated-list-entries
          (nreverse (asdf--versions asdf--list-plugin asdf--list-show-all)))
    (funcall #'tabulated-list-revert)))

(defun asdf-list-toggle-visibility ()
  "Toggle display between all available versions and only those installed."
  (interactive)
  (when (eq major-mode 'asdf-list-mode)
    (setq asdf--list-show-all (not asdf--list-show-all))
    (asdf-list-revert)))

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

(defun asdf-list-use (plugin version &optional local)
  "Switch asdf PLUGIN current version to VERSION at point in `asdf-list-mode'.
If LOCAL, use version locally."
  (interactive (asdf--list-name/version))
  (with-asdf-output (if local "local" "global") plugin version
    (with-current-buffer (asdf-list-buffer) (asdf-list-revert))
    (asdf-message "%s %s using %s" (if local "local" "global") plugin version))
  (run-hooks 'asdf-after-use-hook))

(defun asdf-list-use-local (plugin version)
  "Use PLUGIN VERSION locally."
  (interactive (asdf--list-name/version))
  (asdf-list-use plugin version 'local))

(defun asdf-list-uninstall (plugin version)
  "Uninstall PLUGIN VERSION."
  (interactive (asdf--list-name/version))
  (and (y-or-n-p (format "Uninstall %s %s?" plugin version))
       (with-asdf-output "uninstall" plugin version
         (with-current-buffer (asdf-list-buffer) (asdf-list-revert))
         (asdf-message "uninstalled %s %s" plugin version))))

(defun asdf-list-where (plugin version)
  "Get install path for PLUGIN VERSION."
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
    ["Toggle visibility" asdf-list-toggle-visibility t]
    ["Revert" asdf-list-revert t]))

(defvar asdf-list-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil asdf-list-menu)
    (define-key km "j"           #'forward-line)
    (define-key km "k"           #'previous-line)
    (define-key km (kbd "RET")   #'asdf-list-install)
    (define-key km "i"           #'asdf-list-install)
    (define-key km "u"           #'asdf-list-use)
    (define-key km "l"           #'asdf-list-use-local)
    (define-key km "d"           #'asdf-list-uninstall)
    (define-key km "w"           #'asdf-list-where)
    (define-key km "t"           #'asdf-list-toggle-visibility)
    (define-key km (kbd "M-C-o") #'asdf-list-toggle-visibility)
    km))

(define-derived-mode asdf-list-mode tabulated-list-mode "asdf"
  "List of available asdf versions/plugins.
Commands: \n
\\{asdf-list-mode-map}"
  (tabulated-list-init-header)
  (tabulated-list-print)
  (setq-local revert-buffer-function #'asdf-list-revert))

(define-derived-mode asdf-process-mode nil "asdf-process"
  nil
  (setq mode-line-process '(":%s")))

(provide 'asdf)
;;; asdf.el ends here
