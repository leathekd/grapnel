;;; grapnel.el --- HTTP request lib with flexible callback dispatch

;; Copyright © 2013  David Leatherman

;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/grapnel
;; Version: 0.5.0

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

;; 0.5.0 - Initial release.

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

(defun grapnel-url-escape (str)
  "URI encode STR"
  (let ((str (if (stringp str)
                 str
               (prin1-to-string str))))
    (url-hexify-string
     (encode-coding-string str 'utf-8))))

(defun grapnel-format-params (params)
  "Convert an alist of params into an & delimeted string suitable for curl"
  (mapconcat
   (lambda (p)
     (cond
      ((consp p)
       (concat (grapnel-url-escape (car p)) "=" (grapnel-url-escape (cdr p))))
      (t (grapnel-url-escape p))))
   params "&"))

(defun grapnel-command (url &optional
                            request-method url-params
                            request-data request-headers)
  "Converts the passed arguments into the curl command"
  (let* ((method (or request-method "GET"))
         (url (if (null url-params)
                  url
                (concat url
                        (if (string-match-p "\?" url) "&" "?")
                        (grapnel-format-params url-params))))
         (data (if (null request-data)
                   ""
                 "-d @-"))
         (headers (if (and (equal "POST" request-method)
                           (null (cdr (assoc "Content-Length"
                                             request-headers))))
                      (cons `("Content-Length" . ,(length request-data))
                            request-headers)
                    request-headers))
         (headers (if (null headers)
                      ""
                    (mapconcat
                     (lambda (header-pair)
                       (format "-H '%s: %s'"
                               (car header-pair) (cdr header-pair)))
                     headers
                     " "))))
    (format "%s %s %s -i -s -X %s %s '%s' "
            grapnel-program grapnel-options headers method data url)))

(defun grapnel-parse-headers (header-str)
  "Extracts the response code and converts the headers into an alist"
  (when header-str
    (let ((split-headers (split-string header-str "\n" t)))
      (cons
       (list "response-code"
             (progn (string-match "\\([[:digit:]][[:digit:]][[:digit:]]\\)"
                                  (car split-headers))
                    (string-to-number (match-string 1 (car split-headers)))))
       (mapcar (lambda (line) (split-string line ": "))
               (cdr split-headers))))))

(defun grapnel-response-headers ()
  "Extract the headers from the response buffer"
  (unless (= (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "[\r]" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    (let ((pos (search-forward-regexp "^$" nil t)))
      (when pos
        (let ((headers (buffer-substring (point-min) pos)))
          (delete-region (point-min) (1+ pos))
          headers)))))

(defun grapnel-callback-dispatch (handler-alist exit-code response headers)
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
  (let ((response-code (cadr (assoc "response-code" headers))))
    (cond
     ;; curl error
     ((not (= 0 exit-code))
      (apply (cdr (assoc 'error handler-alist))
             (list response exit-code)))

     ;; response code
     ((assoc response-code handler-alist)
      (apply (cdr (assoc response-code handler-alist))
             (list response headers)))

     ;; success
     ((and (assoc 'success handler-alist)
           (<= 200 response-code)
           (< response-code 300))
      (apply (cdr (assoc 'success handler-alist))
             (list response headers)))

     ;; failure
     ((and (assoc 'failure handler-alist)
           (<= 400 response-code)
           (< response-code 600))
      (apply (cdr (assoc 'failure handler-alist))
             (list response headers)))

     ;; complete (both success and failure)
     ((assoc 'complete handler-alist)
      (apply (cdr (assoc 'complete handler-alist))
             (list response headers))))))

(defun grapnel-sentinel (handler-alist buffer-name process signal)
  "Sentinel function that watches the curl process"
  (when (or (string-match "^finished" signal)
            (string-match "^exited abnormally" signal))
    (with-current-buffer buffer-name
      (let ((headers (grapnel-parse-headers (grapnel-response-headers)))
            (response (buffer-string)))
        (funcall grapnel-callback-dispatch-fn
                 handler-alist (process-exit-status process) response headers))
      (kill-buffer buffer-name))))

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
  (let* ((data (when request-data
                 (if (listp request-data)
                     (grapnel-format-params request-data)
                   request-data)))
         (command (grapnel-command url request-method url-params data
                                   request-headers))
         (buffer-name (generate-new-buffer-name " grapnel"))
         (proc (start-process-shell-command
                "grapnel" buffer-name command)))
    (set-process-sentinel proc (apply-partially 'grapnel-sentinel
                                                handler-alist buffer-name))
    (when request-data
      (process-send-string proc data)
      (process-send-string proc "\n")
      (process-send-eof proc))
    nil))

;;;###autoload
(defun grapnel-retrieve-url-sync (url handler-alist &optional
                                      request-method url-params
                                      request-data request-headers)
  "Behaves the same as `grapnel-retrieve-url' but synchronously."
  (let* ((data (when request-data
                 (if (listp request-data)
                     (grapnel-format-params request-data)
                   request-data)))
         (command (grapnel-command url request-method url-params
                                   data request-headers))
         (buffer-name (generate-new-buffer-name " grapnel"))
         (resp (shell-command-to-string command))
         (exit-code
          (with-temp-buffer
            (insert data)
            (call-process-shell-command command (current-buffer) buffer-name
                                        nil))))
    (with-current-buffer buffer-name
      (let* ((headers (grapnel-parse-headers (grapnel-response-headers)))
             (response (buffer-string))
             (ret (funcall grapnel-callback-dispatch-fn
                           handler-alist exit-code response headers)))
        (kill-buffer buffer-name)
        ret))))

(provide 'grapnel)

;;; grapnel ends here
