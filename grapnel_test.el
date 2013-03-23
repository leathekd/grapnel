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
  (should (equal "fire%20%3d%3f%2bfly"
                 (grapnel-url-escape "fire =?+fly"))))
(ert-deftest grapnel-test-format-params ()
  (should (equal "fire=fly&brown=coat"
                 (grapnel-format-params '(("fire" . "fly")
                                          ("brown" . "coat"))))))

(ert-deftest grapnel-test-command ()
  (let ((grapnel-program "curl"))
    (should (equal "curl -i -s -X GET 'http://www.google.com'"
                   (grapnel-command "http://www.google.com")))
    (should (equal (concat "curl -H 'Content-Length: 0' -i -s"
                           " -X POST 'http://www.google.com'")
                   (grapnel-command "http://www.google.com" "POST")))
    (should (equal (concat "curl -H 'Content-Length: 0' -i -s"
                           " -X POST 'http://www.google.com?q=serenity'")
                   (grapnel-command "http://www.google.com" "POST"
                                    '(("q" . "serenity")))))
    (should (equal (concat "curl -H 'Content-Length: 1' -i -s"
                           " -X POST -d @- 'http://www.google.com?q=serenity'")
                   (grapnel-command "http://www.google.com" "POST"
                                    '(("q" . "serenity"))
                                    '(("doesn't" . "matter")))))
    (should (equal (concat "curl -H 'Content-Length: 1'"
                           " -H 'header: value' -i -s -X POST -d @-"
                           " 'http://www.google.com?q=serenity'")
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
    (insert "Grapnel test file")))

(ert-deftest grapnel-test-retrieve-url-sync ()
  (should (equal "success"
                 (grapnel-retrieve-url-sync
                  "www.google.com"
                  '((complete . (lambda (resp hdrs) "success")))))))

(ert-deftest grapnel-test-retrieve-url ()
  (let* ((tmp-file (format "/tmp/grapnel-test%s.tmp" (random t)))
         (tries 10))
    (unwind-protect
        (progn
          (grapnel-retrieve-url
           "www.google.com"
           `((complete . ,(apply-partially 'grapnel-test-callback tmp-file))))
          ;; kinda gross and will possibly fail on slow connections. :/
          (while (and (not (file-exists-p tmp-file)) (< 0 tries))
            (setq tries (- tries 1))
            (sleep-for 0.5))
          (should (file-exists-p tmp-file)))
      (condition-case err
          (delete-file tmp-file)
        (error nil)))))
