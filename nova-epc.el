;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

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

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro nova-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar nova-deferred-debug nil
  "Debug output switch.")

(defvar nova-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun nova-deferred-log (&rest args)
  "[internal] Debug log function."
  (when nova-deferred-debug
    (with-current-buffer (get-buffer-create "*nova-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" nova-deferred-debug-count (apply #'format args)))))
    (cl-incf nova-deferred-debug-count)))

(defvar nova-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro nova-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`nova-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal nova-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar nova-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar nova-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `nova-deferred-post-task' and `nova-deferred-worker'.")

(defun nova-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`nova-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack nova-deferred-queue)
    (nova-deferred-log "QUEUE-POST [%s]: %s" (length nova-deferred-queue) pack)
    (run-at-time nova-deferred-tick-time nil 'nova-deferred-worker)
    d))

(defun nova-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when nova-deferred-queue
    (let* ((pack (car (last nova-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq nova-deferred-queue (nbutlast nova-deferred-queue))
      (condition-case err
          (setq value (nova-deferred-exec-task d which arg))
        (error
         (nova-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: nova-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `nova-deferred-resignal')
;; cancel      : a canceling function (default `nova-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct nova-deferred-object
  (callback 'identity)
  (errorback 'nova-deferred-resignal)
  (cancel 'nova-deferred-default-cancel)
  next status value)

(defun nova-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun nova-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (nova-deferred-log "CANCEL : %s" d)
  (setf (nova-deferred-object-callback d) 'identity)
  (setf (nova-deferred-object-errorback d) 'nova-deferred-resignal)
  (setf (nova-deferred-object-next d) nil)
  d)

(defun nova-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (nova-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "nova-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (nova-deferred-object-callback d)
                    (nova-deferred-object-errorback d)))
        (next-deferred (nova-deferred-object-next d)))
    (cond
     (callback
      (nova-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((nova-deferred-object-p value)
                                             (nova-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (nova-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (nova-deferred-post-task next-deferred 'ok value)
                                               (setf (nova-deferred-object-status d) 'ok)
                                               (setf (nova-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (nova-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (nova-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (nova-deferred-object-status d) 'ng)
                                            (setf (nova-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (nova-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (nova-deferred-resignal arg)))))))

(defun nova-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (nova-deferred-object-next prev) next)
  (cond
   ((eq 'ok (nova-deferred-object-status prev))
    (setf (nova-deferred-object-status prev) nil)
    (let ((ret (nova-deferred-exec-task
                next 'ok (nova-deferred-object-value prev))))
      (if (nova-deferred-object-p ret) ret
        next)))
   ((eq 'ng (nova-deferred-object-status prev))
    (setf (nova-deferred-object-status prev) nil)
    (let ((ret (nova-deferred-exec-task next 'ng (nova-deferred-object-value prev))))
      (if (nova-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun nova-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-nova-deferred-object :callback callback)
    (make-nova-deferred-object)))

(defun nova-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (nova-deferred-exec-task d 'ok arg))

(defun nova-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (nova-deferred-exec-task d 'ng arg))

(defun nova-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (nova-deferred-post-task d 'ok arg))

(defun nova-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (nova-deferred-callback-post (nova-deferred-new callback))."
  (let ((d (if callback
               (make-nova-deferred-object :callback callback)
             (make-nova-deferred-object))))
    (nova-deferred-callback-post d arg)
    d))

(defun nova-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-nova-deferred-object :callback callback)))
    (nova-deferred-set-next d nd)))

(defun nova-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-nova-deferred-object :errorback callback)))
    (nova-deferred-set-next d nd)))

(defvar nova-epc-debug nil)

(defun nova-epc-log (&rest args)
  (when nova-epc-debug
    (with-current-buffer (get-buffer-create "*nova-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun nova-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar nova-epc-uid 1)

(defun nova-epc-uid ()
  (cl-incf nova-epc-uid))

(defvar nova-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct nova-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun nova-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return nova-epc-connection object."
  (nova-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (nova-epc-uid))
         (connection-name (format "nova-epc con %s" connection-id))
         (connection-buf (nova-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-nova-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (nova-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (nova-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (nova-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun nova-epc-process-sentinel (connection process msg)
  (nova-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (nova-epc-connection-name connection) process msg)
  (nova-epc-disconnect connection))

(defun nova-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (nova-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (nova-epc-connection-process connection)))
    (nova-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun nova-epc-disconnect (connection)
  (let ((process (nova-epc-connection-process connection))
        (buf (nova-epc-connection-buffer connection))
        (name (nova-epc-connection-name connection)))
    (nova-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (nova-epc-log "!! Disconnected finished [%s]" name)))

(defun nova-epc-process-filter (connection process message)
  (nova-epc-log "INCOMING: [%s] [%S]" (nova-epc-connection-name connection) message)
  (with-current-buffer (nova-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (nova-epc-process-available-input connection process)))

(defun nova-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (nova-deferred-new callback)
             (nova-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun nova-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (nova-deferred-callback-post d event))))

(defun nova-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (nova-epc-net-have-input-p)
      (let ((event (nova-epc-net-read-or-lose process))
            (ok nil))
        (nova-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'nova-epc-signal-send
                         (cons (nova-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (nova-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (nova-epc-process-available-input connection process)))))))

(defun nova-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (nova-epc-net-decode-length))))

(defun nova-epc-net-read-or-lose (_process)
  (condition-case error
      (nova-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun nova-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (nova-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun nova-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun nova-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct nova-epc-manager
  "Root object that holds all information related to an EPC activity.

`nova-epc-start-epc' returns this object.

title          : instance name for displaying on the `nova-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : nova-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct nova-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar nova-epc-live-connections nil
  "[internal] A list of `nova-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun nova-epc-server-process-name (uid)
  (format "nova-epc-server:%s" uid))

(defun nova-epc-server-buffer-name (uid)
  (format " *%s*" (nova-epc-server-process-name uid)))

(defun nova-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (nova-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (nova-epc-disconnect (nova-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 nova-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq nova-epc-live-connections (delete mngr nova-epc-live-connections))
    ))

(defun nova-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun nova-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an nova-epc-connection instance."
  (let* ((mngr mngr)
         (conn (nova-epc-manager-connection mngr))
         (channel (nova-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (nova-epc-log "SIG CALL: %S" args)
                    (apply 'nova-epc-handler-called-method ,mngr (nova-epc-args args))))
               (return
                . (lambda (args)
                    (nova-epc-log "SIG RET: %S" args)
                    (apply 'nova-epc-handler-return ,mngr (nova-epc-args args))))
               (return-error
                . (lambda (args)
                    (nova-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'nova-epc-handler-return-error ,mngr (nova-epc-args args))))
               (epc-error
                . (lambda (args)
                    (nova-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'nova-epc-handler-epc-error ,mngr (nova-epc-args args))))
               (methods
                . (lambda (args)
                    (nova-epc-log "SIG METHODS: %S" args)
                    (nova-epc-handler-methods ,mngr (caadr args))))
               ) do
             (nova-epc-signal-connect channel method body))
    (push mngr nova-epc-live-connections)
    mngr))

(defun nova-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (nova-epc-manager-connection mngr)))
    (nova-epc-net-send conn (cons method messages))))

(defun nova-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (nova-epc-manager-methods mngr)
           if (eq method-name (nova-epc-method-name i))
           do (cl-return i)))

(defun nova-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (nova-epc-manager-methods mngr)
                  collect
                  (list
                   (nova-epc-method-name i)
                   (or (nova-epc-method-arg-specs i) "")
                   (or (nova-epc-method-docstring i) "")))))
    (nova-epc-manager-send mngr 'return uid info)))

(defun nova-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (nova-epc-manager-methods mngr))
           (method (nova-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (nova-epc-log "ERR: No such method : %s" name)
        (nova-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (nova-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((nova-deferred-object-p ret)
                (nova-deferred-nextc ret
                                          (lambda (xx) (nova-epc-manager-send mngr 'return uid xx))))
               (t (nova-epc-manager-send mngr 'return uid ret))))
          (error
           (nova-epc-log "ERROR : %S" err)
           (nova-epc-manager-send mngr 'return-error uid err))))))))

(defun nova-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (nova-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (nova-epc-manager-sessions mngr) ret)))

(defun nova-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (nova-epc-manager-sessions mngr))))
    (cond
     (pair
      (nova-epc-log "RET: id:%s [%S]" uid args)
      (nova-epc-manager-remove-session mngr uid)
      (nova-deferred-callback (cdr pair) args))
     (t                                 ; error
      (nova-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun nova-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (nova-epc-manager-sessions mngr))))
    (cond
     (pair
      (nova-epc-log "RET-ERR: id:%s [%S]" uid args)
      (nova-epc-manager-remove-session mngr uid)
      (nova-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (nova-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun nova-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (nova-epc-manager-sessions mngr))))
    (cond
     (pair
      (nova-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (nova-epc-manager-remove-session mngr uid)
      (nova-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (nova-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun nova-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (nova-epc-uid))
        (sessions (nova-epc-manager-sessions mngr))
        (d (nova-deferred-new)))
    (push (cons uid d) sessions)
    (setf (nova-epc-manager-sessions mngr) sessions)
    (nova-epc-manager-send mngr 'call uid method-name args)
    d))

(defun nova-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-nova-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (nova-epc-manager-methods mngr))))
    (setf (nova-epc-manager-methods mngr) methods)
    method))

(defun nova-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'nova-epc-nothing))
    (nova-deferred-chain
     d
     (nova-deferred-nextc it
                               (lambda (x) (setq result x)))
     (nova-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'nova-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (nova-epc-connection-process (nova-epc-manager-connection mngr))
         0 nova-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun nova-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (nova-epc-sync mngr (nova-epc-call-deferred mngr method-name args)))

(defun nova-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (nova-epc-connection-process (nova-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar nova-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`nova-epc-manager' instance]).
When the server process accepts the client connection, the
`nova-epc-manager' instance is created and stored in this variable
`nova-epc-server-client-processes'. This variable is used for the management
purpose.")

;; nova-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `nova-epc-manager' instances
(cl-defstruct nova-epc-server name process port connect-function)

(defvar nova-epc-server-processes nil
  "[internal] A list of ([process object] . [`nova-epc-server' instance]).
This variable is used for the management purpose.")

(defun nova-epc-server-get-manager-by-process (proc)
  "[internal] Return the nova-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in nova-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun nova-epc-server-accept (process)
  "[internal] Initialize the process and return nova-epc-manager object."
  (nova-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (nova-epc-uid))
         (connection-name (format "nova-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-nova-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (nova-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (nova-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (nova-epc-process-sentinel connection p e)))
    (make-nova-epc-manager :server-process process :port t
                                :connection connection)))

(defun nova-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (nova-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (nova-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (nova-epc-server-accept process)))
            (push (cons process mngr) nova-epc-server-client-processes)
            (nova-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (nova-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (nova-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process nova-epc-server-client-processes)) _d)
        (when pair
          (nova-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (nova-epc-stop-epc (cdr pair))
          (setq nova-epc-server-client-processes
                (assq-delete-all process nova-epc-server-client-processes))
          ))
      nil))))

(defun nova-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "NOVA EPC Server %s" (nova-epc-uid)))
       (buf (nova-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (nova-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-nova-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          nova-epc-server-processes)
    main-process))

(provide 'nova-epc)
;;; nova-epc.el ends here
