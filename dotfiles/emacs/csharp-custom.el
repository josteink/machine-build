
(defconst csharp-test-delimiter "-")

(defmacro gen-csharp-tests (name &rest body)
  `(let* ((class-name  ,(symbol-name name))
          (buffer-name (concatenate 'string class-name ".cs"))
          (buffer      (generate-new-buffer buffer-name)))
     (switch-to-buffer buffer)
     (insert "using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Test.SuperOffice.SETUP_PROPER_NAMESPACE
{
    [TestClass]
    public class ")
     (insert class-name)
     (newline-and-indent)
     (insert "{")
     (gen-csharp-test-functions "" ',body)

     (insert "}")
     (newline-and-indent)
     (insert "}")
     (kill-ring-save (point-min) (point-max))
     (kill-buffer buffer)))


(defun gen-csharp-test-functions (prefix items)
  (dolist (item items)
    (if (listp item)
        (let* ((item-prefix (symbol-name (car item)))
               (rest        (cdr item))
               (new-prefix  (if (equal "" prefix)
                                item-prefix
                              (concatenate 'string prefix csharp-test-delimiter item-prefix))))
          (gen-csharp-test-functions new-prefix rest))
      (progn
        ;; item does not contain nested items. generate away.
        (gen-csharp-test-function prefix item)))))

(defun gen-csharp-test-function (prefix item)
  (newline-and-indent)
  (insert "      [TestMethod]
      [TestCategory(\"LightningTest\")]
      public void ")
  (insert (gen-csharp-test-name prefix item))
  (insert "()
      {
          throw new NotImplementedException();
      }
"))

(defun gen-csharp-test-function-name (underscored)
  (save-excursion
    (with-temp-buffer
      (insert underscored)
      (capitalize-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun gen-csharp-test-name (prefix item)
  (let* ((base-name   (concatenate 'string prefix csharp-test-delimiter (symbol-name item)))
         (underscored (replace-regexp-in-string csharp-test-delimiter "_" base-name))
         (camelcased  (gen-csharp-test-function-name underscored)))
    camelcased))

;; (gen-csharp-tests filenameurlencodinghelpertests
;;                   (encoding-with no-url-leaves-filename-untouched
;;                                  http-url-should-encode-http-marker
;;                                  https-url-should-include-https-marker
;;                                  custom-port-should-include-port-marker)
;;                   (encoding-is-detectable)
;;                   (decoding detects-http-marker
;;                             detects-https-marker
;;                             detects-port-marker
;;                             resolves-original-url))


;; (gen-csharp-tests SoProtocolModuleTests
;;                   (ignores
;;                    signalr-requests
;;                    fileupload-requests
;;                    filedownload-requsets)
;;                (processes
;;                 soprotocol-requests))




(gen-csharp-test-function-name "a-b-cde-fgh-fg")
;; Test-Boo-Hiis
