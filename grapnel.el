;;; grapnel.el --- HTTP request lib with flexible callback dispatch

;; Copyright © 2013  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grapnel
;; Version: 0.5.2

;; This file is not part of GNU Emacs.

;;; Commentary:

;; grapnel is an HTTP request library that uses a curl subprocess and
;; offers flexible callback dispatch.  Not only can you pass in an
;; alist of request outcomes to callback functions (see below) but you
;; can also override the dispatch function itself if the default one
;; doesn't suit your needs.  Further, grapnel will build the query
;; string, request data (i.e., POST body), and headers from alists
;; that are passed in.

;; An example:
;; (grapnel-retrieve-url
;;  "www.google.com"
;;  '((success . (lambda (res hdrs) (message "%s" res)))
;;    (failure . (lambda (res hdrs) (message "Fail: %s" res)))
;;    (error   . (lambda (res err)  (message "Err: %s" err))))
;;  "GET"
;;  '((q . "ASIN B001EN71CW")))

;; History

;; 0.5.1 - Initial release.

;; 0.5.2 - Fix some quoting issues on Windows

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(defvar grapnel-program (executable-find "curl")
  "Path to the curl executable")

(defvar grapnel-options ""
  "Additional options to pass to all curl requests.  Enter them just as they
would be entered on the command line.")

(defvar grapnel-callback-dispatch-fn 'grapnel-callback-dispatch
  "Dispatch function for handling the curl response.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Debug related functions

(defvar grapnel-debug-buffer "*grapnel-debug-log*"
  "The buffer into which to log grapnel related debug log messages")

(defvar grapnel-debug-p nil
  "Boolean that determines if grapnel should output debug info into
the `grapnel-debug-buffer'")

(defun grapnel-log (&rest args)
  "Converts arguments into strings and inserts them, space delimited,
into `grapnel-debug-buffer' only if `grapnel-debug-p' is truthy."
  (when grapnel-debug-p
    (with-current-buffer (get-buffer-create grapnel-debug-buffer)
      (goto-char (point-max))
      (insert (mapconcat 'prin1-to-string args " ") "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Request oriented middleware

(defun grapnel-add-request-method (req-alist)
  "Adds the HTTP method to an alist under the request key inside
REQ-ALIST.  Returns the combined alist."
  (let ((method (a-get req-alist 'method)))
    (a-put-in req-alist '(request method)
              (or method "GET"))))

(defun grapnel-url-escape (str)
  "URI encode STR"
  (url-hexify-string
   (encode-coding-string (format "%s" str) 'utf-8)))

(defun grapnel-format-params (params)
  "Convert an alist of params into an & delimeted string suitable for curl"
  (mapconcat
   (lambda (p)
     (cond
      ((consp p)
       (concat (grapnel-url-escape (car p)) "=" (grapnel-url-escape (cdr p))))
      (t (grapnel-url-escape p))))
   params "&"))

(defun grapnel-add-request-params (req-alist)
  "Formats the given url params from the 'params key in REQ-ALIST and
adds the result to an alist under the request key inside REQ-ALIST.
Returns the combined alist."
  (let ((params (a-get req-alist 'params)))
    (a-put-in req-alist '(request params)
              (when params
                (grapnel-format-params params)))))

(defun grapnel-add-request-url (req-alist)
  "Combines the url in REQ-ALIST with the params in the request
sub-alist into the actual url that will be used in the curl request.
Adds this new url into the requests sub-alist.

Should be called after `grapnel-add-request-params'."
  (let ((params (a-get-in req-alist '(request params))))
    (a-put-in req-alist '(request url)
              (cond ((null params) url)
                    ((string-match-p "\?" url) (format "%s&%s" url params))
                    (t (format "%s?%s" url params))))))

(defun grapnel-add-request-data (req-alist)
  "Formats the given request data from the 'data key in REQ-ALIST and
adds the result to an alist under the request key inside REQ-ALIST.
Returns the combined alist."
  (let ((data (a-get req-alist 'data)))
    (a-put-in req-alist '(request data)
              (when data
                (if (listp data)
                    (grapnel-format-params data)
                  data)))))

(defun grapnel-add-request-headers (req-alist)
  "Formats the given request headers from the 'headers key in
REQ-ALIST and adds the result to an alist under the request key inside
REQ-ALIST.  Returns the combined alist.

If called after `grapnel-add-request-data' and no 'Content-Length'
header is found, a 'Content-Length' header will be added."
  (let ((data (a-get-in req-alist '(request data)))
        (headers (a-get req-alist 'headers)))
    (a-put-in req-alist '(request headers)
              (if (and (null (a-get headers "Content-Length"))
                       (null (a-get headers "content-length")))
                  (a-put headers "Content-Length" (length data))
                headers))))

(defun grapnel-command (url &optional
                            method params
                            data headers
                            options)
  "Converts the passed arguments into the curl command"
  (let* ((data (if data
                   " --data @-"
                 ""))
         (headers (if (null headers)
                      ""
                    (mapconcat
                     (lambda (header-pair)
                       (format " --header %s"
                               (shell-quote-argument
                                (format "%s: %s"
                                        (car header-pair)
                                        (cdr header-pair)))))
                     headers
                     "")))
         (options (if options
                      (concat " " options)
                    ""))
         (method (if method
                     (format " --request %s" method)
                   "")))
    (format "%s%s%s --include --silent%s%s %s"
            grapnel-program options headers method data
            (shell-quote-argument url))))

(defun grapnel-add-request-command (req-alist)
  "Formats and adds the curl command to the request alist in
REQ-ALIST.  Should be called after basically everything else:
`grapnel-add-request-url'
`grapnel-add-request-method'
`grapnel-add-request-params'
`grapnel-add-request-data'
`grapnel-add-request-headers'"
  (a-put-in req-alist '(request command)
            (grapnel-command (a-get-in req-alist '(request url))
                             (a-get-in req-alist '(request method))
                             (a-get-in req-alist '(request params))
                             (a-get-in req-alist '(request data))
                             (a-get-in req-alist '(request headers))
                             (a-get req-alist 'curl-options))))

(defun grapnel-add-request-buffer (req-alist)
  "Creates a name for the buffer that will gather the output of the
curl command.  This is added to the request alist in REQ-ALIST"
  (a-put-in req-alist '(request buffer) (generate-new-buffer-name " grapnel")))

(defvar grapnel-default-request-middleware
  '(grapnel-add-request-buffer
    grapnel-add-request-method
    grapnel-add-request-params
    grapnel-add-request-url
    grapnel-add-request-data
    grapnel-add-request-headers
    grapnel-add-request-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Response oriented middleware

(defun grapnel-add-raw-response-headers (req-alist)
  "Extract the headers from the response buffer and add i to the
response alist in REQ-ALIST"
  (a-put-in req-alist '(response raw-headers)
            (when (< (point-min) (point-max))
              (goto-char (point-min))
              (while (re-search-forward "[\r]" nil t)
                (replace-match "" nil nil))
              (goto-char (point-min))
              (let ((pos (search-forward-regexp "^$" nil t)))
                (when pos
                  (prog1
                      (buffer-substring (point-min) pos)
                    (delete-region (point-min) (1+ pos))))))))

(defun grapnel-add-response-body (req-alist)
  "Extract the response body from the output buffer and add it to the
response alist in REQ-ALIST"
  (a-put-in req-alist '(response body) (buffer-substring-no-properties
                                        (point-min) (point-max))))

(defun grapnel-add-response-code (req-alist)
  "Extract the HTTP response code from the output buffer and add it to
the response alist in REQ-ALIST.  Should be called after
`grapnel-add-raw-response-headers'"
  (save-match-data
    (let* ((raw-headers (a-get-in req-alist '(response raw-headers) ""))
           (split-headers (split-string raw-headers "\n" t))
           (response-code-line (car split-headers)))
      (a-put-in req-alist '(response code)
                (when (< 0 (length raw-headers))
                  (string-match "\\([[:digit:]][[:digit:]][[:digit:]]\\)"
                                response-code-line)
                  (string-to-number (match-string 1 response-code-line)))))))

(defun grapnel-add-response-headers (req-alist)
  "Parses the raw headers in the response alist and adds them to the
response alist in REQ-ALIST.  Should be called after
`grapnel-add-raw-response-headers'"
  (let* ((raw-headers (a-get-in req-alist '(response raw-headers) ""))
         (split-headers (cdr (split-string raw-headers "\n" t))))
    (a-put-in req-alist '(response headers)
              (when (< 0 (length split-headers))
                (mapcar (lambda (line) (split-string line ": "))
                        split-headers)))))

(defvar grapnel-default-response-middleware
  '(grapnel-add-raw-response-headers
    grapnel-add-response-code
    grapnel-add-response-headers
    grapnel-add-response-body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Core request/response functions

(defun grapnel-callback-dispatch (req-alist)
  "Default dispatch function.  Call the first matching function in HANDLER-ALIST
based on the response.  HANDLER-ALIST is in the form:

'((success . (lambda (response response-headers) ...))
  (error . (lambda (response error-code) ...)))

The valid keys in the alist are (in order of precedence):
- 'error - for when the curl call fails in some way
- the HTTP response code as a number
- 'success - any HTTP response 200-299
- 'failure - any HTTP response 400-599
- 'complete - any HTTP response

'error is the only one that is called with (response error-code) all of the
rest of them are called with (response headers)"
  (grapnel-log "grapnel-callback-dispatch" req-alist)
  (let ((handler-alist (a-get req-alist 'handlers))
        (response-code (a-get-in req-alist '(response code)))
        (response (a-get-in req-alist '(response body)))
        (headers (a-get-in req-alist '(response headers)))
        (exit-code (a-get-in req-alist '(response process-exit-code))))
    (grapnel-log "grapnel-callback-dispatch" handler-alist response-code
                 exit-code)
    (cond
     ;; curl error
     ((and (assoc 'error handler-alist)
           (not (= 0 exit-code)))
      (grapnel-log "grapnel-callback-dispatch" "dispatch to error")
      (apply (cdr (assoc 'error handler-alist))
             (list response exit-code)))

     ;; response code
     ((assoc response-code handler-alist)
      (grapnel-log "grapnel-callback-dispatch" "dispatch to" response-code)
      (apply (cdr (assoc response-code handler-alist))
             (list response headers)))

     ;; success
     ((and (assoc 'success handler-alist)
           (<= 200 response-code)
           (< response-code 300))
      (grapnel-log "grapnel-callback-dispatch" "dispatch to success")
      (apply (cdr (assoc 'success handler-alist))
             (list response headers)))

     ;; failure
     ((and (assoc 'failure handler-alist)
           (<= 400 response-code)
           (< response-code 600))
      (grapnel-log "grapnel-callback-dispatch" "dispatch to failure")
      (apply (cdr (assoc 'failure handler-alist))
             (list response headers)))

     ;; complete (both success and failure)
     ((assoc 'complete handler-alist)
      (grapnel-log "grapnel-callback-dispatch" "dispatch to complete")
      (apply (cdr (assoc 'complete handler-alist))
             (list response headers)))

     ;; warn if there was no handler
     (t (message (concat "GRAPNEL WARNING: Missing handler for response. "
                         "Curl exit code: %s, Response code: %s")
                 exit-code response-code)))))

(defun grapnel-process-response (req-alist exit-code)
  "Parses the response and invokes the callback dispatch function."
  (let ((request-buffer (a-get-in req-alist '(request buffer))))
    (funcall grapnel-callback-dispatch-fn
             (prog1
                 (with-current-buffer request-buffer
                   (-reduce-from (lambda (resp-alist fn)
                                   (funcall fn resp-alist))
                                 (a-put-in req-alist
                                           '(response process-exit-code)
                                           exit-code)
                                 (a-get req-alist 'response-middleware)))
               (kill-buffer request-buffer)))))

(defun grapnel-sentinel (req-alist process signal)
  "Sentinel function that watches the async curl process"
  (when (or (string-match "^finished" signal)
            (string-match "^exited abnormally" signal))
    (grapnel-process-response req-alist (process-exit-status process))))

(defun grapnel-wait-for-process (proc)
  "Busy waits until PROC has finished effectively making it a sync
process."
  (while (and (processp proc)
              (process-live-p proc))
    (grapnel-log "grapnel-wait-for-process" (processp proc)
                 (process-live-p proc)
                 (process-status proc)
                 (process-command proc))
    (sit-for 0.25)))

(defun grapnel-request (req-alist)
  "Internal base request function.  See `grapnel-retrieve-url' for
proper documentation."
  (grapnel-log "grapnel-request" req-alist)
  (let* ((request-alist (-reduce-from (lambda (alist fn)
                                        (funcall fn alist))
                                      req-alist
                                      (a-get req-alist 'request-middleware)))
         (command (a-get-in request-alist '(request command)))
         (request-buffer (a-get-in request-alist '(request buffer)))
         (proc (start-process-shell-command "grapnel" request-buffer command))
         (data (a-get-in request-alist '(request data)))
         (handler-alist (a-get request-alist 'handlers)))
    (grapnel-log "request-alist" request-alist)
    (when data
      (process-send-string proc data)
      (process-send-string proc "\n")
      (process-send-eof proc))
    (if (a-get request-alist 'sync)
        (progn
          (grapnel-wait-for-process proc)
          (grapnel-process-response request-alist (process-exit-status proc)))
      (progn
        (set-process-sentinel proc (apply-partially 'grapnel-sentinel
                                                    request-alist))
        nil))))

(defun grapnel-default-handler (&rest args)
  "Default response handler that simply echos the responses via `message'"
  (message "Default grapnel handler output: %s"
           (mapconcat 'prin1-to-string args "\n")))

(defvar grapnel-default-request-alist
  `((url . "")
    (sync . nil)
    (handlers . ((complete . grapnel-default-handler)
                 (error . grapnel-default-handler)))
    (method . "GET")
    (params . nil)
    (data . nil)
    (headers . nil)
    (curl-options . nil)
    (request-middleware . ,grapnel-default-request-middleware)
    (response-middleware . ,grapnel-default-response-middleware))
  "The request alist with some simple default values.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Client functions

;;;###autoload
(defun grapnel-get (url opts-alist)
  "Simple HTTP GET call.  OPTS-ALIST can contain any of the keys found
in `grapnel-default-request-alist' to override those defaults."
  (grapnel-request
   (a-merge grapnel-default-request-alist
            opts-alist
            `((url . ,url)
              (method . "GET")))))

;;;###autoload
(defun grapnel-post (url opts-alist)
  "Simple HTTP POST call.  OPTS-ALIST can contain any of the keys found
in `grapnel-default-request-alist' to override those defaults."
  (grapnel-request
   (a-merge grapnel-default-request-alist
            opts-alist
            `((url . ,url)
              (method . "POST")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Old request format left here for compatibility

(defun grapnel-make-request-alist (url handler-alist sync request-method
                                       url-params request-data request-headers)
  "Transitional utility function that wraps the grapnel-retrieve-url*
function arguments in an alist. "
  (a-merge
   grapnel-default-request-alist
   `((url . ,url)
     (sync . ,sync)
     (handlers . ,handler-alist)
     (method . ,request-method)
     (params . ,url-params)
     (data . ,request-data)
     (headers . ,request-headers))))

;;;###autoload
(defun grapnel-retrieve-url (url handler-alist &optional
                                 request-method url-params
                                 request-data request-headers)
  "Retrieve URL asynchronously and call the corresponding handler in
HANDLER-ALIST.  See the documentation for `grapnel-callback-dispatch' for
details on HANDLER-ALIST.

REQUEST-METHOD: a string and can be any valid HTTP verb
URL-PARAMS: an alist and will be formatted into a query string and url encoded
REQUEST-DATA: an alist, automatically formatted and urlencoded, sent over stdin
REQUEST-HEADERS: an alist of header name to value pairs"
  (grapnel-request (grapnel-make-request-alist url handler-alist nil
                                               request-method url-params
                                               request-data request-headers)))

;;;###autoload
(defun grapnel-retrieve-url-sync (url handler-alist &optional
                                      request-method url-params
                                      request-data request-headers)
  "Behaves the same as `grapnel-retrieve-url' but busy waits for the
underlying curl process to finish."
  (grapnel-log "grapnel-retrieve-url-sync" url handler-alist request-method
               url-params request-data request-headers)
  (grapnel-request (grapnel-make-request-alist url handler-alist t
                                               request-method url-params
                                               request-data request-headers)))

(provide 'grapnel)

;;; grapnel.el ends here
