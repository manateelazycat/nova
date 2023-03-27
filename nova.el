;;; nova.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: nova.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2022-10-10 15:23:53 +0800
;;           By: Gong Qijian
;; URL: https://github.com/manateelazycat/nova
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Nova
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET nova RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'nova-epc)

(defgroup nova nil
  "Nova group."
  :group 'applications)

(defvar nova-server nil
  "The Nova Server.")

(defvar nova-python-file (expand-file-name "nova.py" (if load-file-name
                                                         (file-name-directory load-file-name)
                                                       default-directory)))

(defvar nova-server-port nil)

(defun nova--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p nova-server)
    (setq nova-server
          (nova-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (nova-epc-define-method mngr 'eval-in-emacs 'nova--eval-in-emacs-func)
               (nova-epc-define-method mngr 'get-emacs-var 'nova--get-emacs-var-func)
               (nova-epc-define-method mngr 'get-emacs-vars 'nova--get-emacs-vars-func)
               (nova-epc-define-method mngr 'get-user-emacs-directory 'nova--user-emacs-directory)
               ))))
    (if nova-server
        (setq nova-server-port (process-contact nova-server :service))
      (error "[Nova] nova-server failed to start")))
  nova-server)

(defun nova--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun nova--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun nova--get-emacs-vars-func (&rest vars)
  (mapcar #'nova--get-emacs-var-func vars))

(defvar nova-epc-process nil)

(defvar nova-internal-process nil)
(defvar nova-internal-process-prog nil)
(defvar nova-internal-process-args nil)

(defcustom nova-name "*nova*"
  "Name of Nova buffer."
  :type 'string)

(defcustom nova-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run nova.py."
  :type 'string)

(defcustom nova-enable-debug nil
  "If you got segfault error, please turn this option.
Then Nova will start by gdb, please send new issue with `*nova*' buffer content when next crash."
  :type 'boolean)

(defcustom nova-enable-log nil
  "Enable this option to print log message in `*nova*' buffer, default only print message header."
  :type 'boolean)

(defcustom nova-enable-profile nil
  "Enable this option to output performance data to ~/nova.prof."
  :type 'boolean)

(defun nova--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun nova-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (nova-epc-live-p nova-epc-process)
      (nova-deferred-chain
        (nova-epc-call-deferred nova-epc-process (read method) args))
    (setq nova-first-call-method method)
    (setq nova-first-call-args args)
    (nova-start-process)))

(defvar nova-is-starting nil)
(defvar nova-first-call-method nil)
(defvar nova-first-call-args nil)

(defun nova-restart-process ()
  "Stop and restart Nova process."
  (interactive)
  (setq nova-is-starting nil)

  (nova-kill-process)
  (nova-start-process)
  (message "[Nova] Process restarted."))

(defun nova-start-process ()
  "Start Nova process if it isn't started."
  (setq nova-is-starting t)
  (unless (nova-epc-live-p nova-epc-process)
    ;; start epc server and set `nova-server-port'
    (nova--start-epc-server)
    (let* ((nova-args (append
                       (list nova-python-file)
                       (list (number-to-string nova-server-port))
                       (when nova-enable-profile
                         (list "profile"))
                       )))

      ;; Set process arguments.
      (if nova-enable-debug
          (progn
            (setq nova-internal-process-prog "gdb")
            (setq nova-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" nova-python-command) nova-args)))
        (setq nova-internal-process-prog nova-python-command)
        (setq nova-internal-process-args nova-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq nova-internal-process
              (apply 'start-process
                     nova-name nova-name
                     nova-internal-process-prog nova-internal-process-args)))
      (set-process-query-on-exit-flag nova-internal-process nil))))

(defvar nova-stop-process-hook nil)

(defvar-local nova-is-remote-file nil)
(defvar-local nova-remote-file-host nil)
(defvar-local nova-remote-file-path nil)

(defun nova-kill-process ()
  "Stop Nova process and kill all Nova buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'nova-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (nova--kill-python-process))

(add-hook 'kill-emacs-hook #'nova-kill-process)

(defun nova--kill-python-process ()
  "Kill Nova background python process."
  (when (nova-epc-live-p nova-epc-process)
    ;; Cleanup before exit Nova server process.
    (nova-call-async "cleanup")
    ;; Delete Nova server process.
    (nova-epc-stop-epc nova-epc-process)
    ;; Kill *nova* buffer.
    (when (get-buffer nova-name)
      (kill-buffer nova-name))
    (setq nova-epc-process nil)
    (message "[Nova] Process terminated.")))

(defun nova--first-start (nova-epc-port)
  "Call `nova--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq nova-epc-process (make-nova-epc-manager
                          :server-process nova-internal-process
                          :commands (cons nova-internal-process-prog nova-internal-process-args)
                          :title (mapconcat 'identity (cons nova-internal-process-prog nova-internal-process-args) " ")
                          :port nova-epc-port
                          :connection (nova-epc-connect "localhost" nova-epc-port)
                          ))
  (nova-epc-init-epc-layer nova-epc-process)
  (setq nova-is-starting nil)

  (when (and nova-first-call-method
             nova-first-call-args)
    (nova-deferred-chain
      (nova-epc-call-deferred nova-epc-process
                              (read nova-first-call-method)
                              nova-first-call-args)
      (setq nova-first-call-method nil)
      (setq nova-first-call-args nil)
      )))

(defun nova-open-file (path)
  (interactive "sPath: ")
  (nova-call-async "open_file" path))

(defun nova-open-file--response(server path content)
  (let ((buf-name (format "nova %s:%s" server path)))
    (with-current-buffer (get-buffer-create buf-name)
      (text-mode)

      (read-only-mode -1)
      (erase-buffer)
      (insert (nova-decode-base64 content))
      (goto-char (point-min))

      (let ((mode (nova-get-mode-name-from-file-path path)))
        (when mode
          (let ((nova-is-remote-file t)
                (nova-remote-file-host server)
                (nova-remote-file-path path))
            (funcall mode)))))

    (switch-to-buffer buf-name)

    (setq-local nova-is-remote-file t)
    (setq-local nova-remote-file-host server)
    (setq-local nova-remote-file-path path)
    ))

(defun nova-get-mode-name-from-file-path (file-path)
  (cdr (assoc file-path
              auto-mode-alist
              'string-match-p)))

(defun nova-decode-base64 (base64-string)
  (decode-coding-string (base64-decode-string base64-string) 'utf-8))

(defun nova-send-lsp-request (method &rest args)
  (pcase method
    ("change_file" (nova-deferred-chain
                     (nova-epc-call-deferred nova-epc-process (read method) (append (list nova-remote-file-host nova-remote-file-path) args))))
    (t (message "Send LSP request for Nova: %s %s" method args))))

(provide 'nova)

;;; nova.el ends here
