(require 'ert)
(require 'grapnel)

(ert-deftest grapnel-test-well-documented ()
  (let ((needs-docs nil))
    (mapatoms (lambda (x)
                (when (and (fboundp x)
                           (string-match "^grapnel" (symbol-name x))
                           (or (not (documentation x t))
                               (string= "" (s-trim (documentation x t)))))
                  (setq needs-docs (cons (symbol-name x) needs-docs)))))
    (should (eql nil (sort needs-docs 'string<)))))

(ert-deftest grapnel-test-url-escape ()
  (should (equal (downcase "fire%20%3d%3f%2bfly")
                 (downcase (grapnel-url-escape "fire =?+fly")))))
(ert-deftest grapnel-test-format-params ()
  (should (equal "fire=fly&brown=coat"
                 (grapnel-format-params '(("fire" . "fly")
                                          ("brown" . "coat"))))))

(ert-deftest grapnel-test-command ()
  (let ((grapnel-program "curl"))
    (should (equal (concat "curl --include --silent --request GET"
                           " http\\://www.google.com")
                   (grapnel-command "http://www.google.com")))
    (should (equal (concat "curl --header Content-Length\\:\\ 0 --include"
                           " --silent --request POST http\\://www.google.com")
                   (grapnel-command "http://www.google.com" "POST")))
    (should (equal (concat "curl --header Content-Length\\:\\ 0 --include"
                           " --silent --request POST"
                           " http\\://www.google.com\\?q\\=serenity")
                   (grapnel-command "http://www.google.com" "POST"
                                    '(("q" . "serenity")))))
    (should (equal (concat "curl --header Content-Length\\:\\ 1 --include"
                           " --silent --request POST --data @-"
                           " http\\://www.google.com\\?q\\=serenity")
                   (grapnel-command "http://www.google.com" "POST"
                                    '(("q" . "serenity"))
                                    '(("doesn't" . "matter")))))
    (should (equal (concat "curl --header Content-Length\\:\\ 1"
                           " --header header\\:\\ value --include --silent"
                           " --request POST --data @-"
                           " http\\://www.google.com\\?q\\=serenity")
                   (grapnel-command "http://www.google.com" "POST"
                                    '(("q" . "serenity"))
                                    '(("doesn't" . "matter"))
                                    '(("header" . "value")))))))

(ert-deftest grapnel-test-parse-headers ()
  (let ((headers (concat
                  "HTTP/1.1 200 OK\n"
                  "Date: Sat, 23 Mar 2013 21:49:44 GMT\n"
                  "Expires: -1\n"
                  "Cache-Control: private, max-age=0\n"
                  "Content-Type: text/html; charset=ISO-8859-1\n")))
    (should (equal (grapnel-parse-headers headers)
                   '(("response-code" 200)
                     ("Date" "Sat, 23 Mar 2013 21:49:44 GMT")
                     ("Expires" "-1")
                     ("Cache-Control" "private, max-age=0")
                     ("Content-Type" "text/html; charset=ISO-8859-1"))))))

(ert-deftest grapnel-test-response-headers ()
  (let ((headers (concat
                  "HTTP/1.1 200 OK\n"
                  "Date: Sat, 23 Mar 2013 21:49:44 GMT\n"
                  "Expires: -1\n"
                  "Cache-Control: private, max-age=0\n"
                  "Content-Type: text/html; charset=ISO-8859-1\n")))
    (with-temp-buffer
      (insert headers
              "\n"
              "body")
      (should (equal headers (grapnel-response-headers))))))

(ert-deftest grapnel-test-callback-dispatch ()
  (let ((dispatch '((201 . (lambda (_ _) "201"))
                    (success . (lambda (_ _) "success"))
                    (failure . (lambda (_ _) "failure"))
                    (complete . (lambda (_ _) "complete"))
                    (error . (lambda (_ _) "error"))))
        (complete '((201 . (lambda (_ _) 201))
                    (complete . (lambda (_ _) "complete"))
                    (error . (lambda (_ _) "error")))))
    (should (equal "success"
                   (grapnel-callback-dispatch dispatch 0 1
                                              '(("response-code" 200)))))
    (should (equal "201"
                   (grapnel-callback-dispatch dispatch 0 1
                                              '(("response-code" 201)))))
    (should (equal "failure"
                   (grapnel-callback-dispatch dispatch 0 1
                                              '(("response-code" 400)))))
    (should (equal "error" (grapnel-callback-dispatch dispatch 1 1 nil)))
    (should (equal "complete"
                   (grapnel-callback-dispatch complete 0 1
                                              '(("response-code" 200)))))
    (should (equal "complete"
                   (grapnel-callback-dispatch complete 0 1
                                              '(("response-code" 400)))))
    ;; TODO: figure out how to test that a warning occurs
    ;;(grapnel-callback-dispatch '() 1 1 '())
    ))

(defun grapnel-test-callback (filename resp headers)
  "Create the file that the test looks for"
  (with-temp-file filename
    (insert resp)))

(ert-deftest grapnel-test-retrieve-url-sync ()
  (should
   (equal "success"
          (grapnel-retrieve-url-sync
           "www.google.com"
           '((success . (lambda (resp hdrs) "success"))
             (failure . (lambda (&rest args) "failure")))))))

(ert-deftest grapnel-test-retrieve-url ()
  (let* ((tmp-file (make-temp-file "grapnel_test" nil "tmp"))
         (tries 10))
    (unwind-protect
        (progn
          (grapnel-retrieve-url
           "www.google.com"
           `((success . ,(apply-partially 'grapnel-test-callback tmp-file))
             (failure . (lambda (&rest args)
                          (should
                           (error "The request should have succeeded"))))))
          ;; kinda gross and will possibly fail on slow connections. :/
          (while (and (not (file-exists-p tmp-file)) (< 0 tries))
            (setq tries (- tries 1))
            (sleep-for 0.5))
          (should (file-exists-p tmp-file)))
      (condition-case err
          (delete-file tmp-file)
        (error nil)))))

(ert-deftest grapnel-test-headers ()
  (let ((opera-mini-resp
         (grapnel-retrieve-url-sync
          "www.google.com"
          `((success . (lambda (resp hdrs) resp))
            (failure . (lambda (&rest args) (error "The request failed"))))
          "GET"
          nil
          nil
          '(("Accept" . (mapconcat 'identity
                                   '("application/vnd.wap.wmlc"
                                     "application/vnd.wap.wmlscriptc"
                                     "image/vnd.wap.wbmp"
                                     "application/vnd.wap.wtls-ca-certificate"
                                     "image/gif"
                                     "text/plain")
                                   ", "))
            ;; this is here just for the embedded quotes
            ("If-None-Match" "\"abc123\"")
            ("User-Agent" . "Nokia8310/1.0 (04.53)"))))
        (firefox-response
         (grapnel-retrieve-url-sync
          "www.google.com"
          `((success . (lambda (resp hdrs) resp))
            (failure . (lambda (&rest args) (error "The request failed"))))
          "GET"
          nil
          nil
          '(("User-Agent" . (concat "User-Agent: Mozilla/5.0"
                                    " (X11; Linux x86_64; rv:12.0)"
                                    " Gecko/20100101 Firefox/21.0"))))))
    (should (string-match "WAPFORUM//DTD" opera-mini-resp))
    (should (> (length firefox-response) (length opera-mini-resp)))))
