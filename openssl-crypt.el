;;; openssl-crypt.el -- Simple inline encryption via OpenSSL

;; Copyright (C) 2013, Daniel Ralston <Wubbulous@gmail.com>
;; Keywords: crypt

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 2 as
;; published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.


(defconst openssl-crypt-args
  '("-a" ; Use base64 encoding
    "-salt" ; Salt, for flavour and security
    "-pass" "stdin")) ; Use the first line of stdin as the password

(defcustom openssl-crypt-command "openssl"
  "The base openssl command to run when encrypting or decrypting.")

(defcustom openssl-crypt-cipher "aes-256-cbc"
  "The default cipher to use when encrypting or decrypting.")


(defun openssl-crypt-setup-input (pass text)
  "Insert the password and main text body into the current
buffer before the point, for input into openssl.

The password must be all by itself on the first line, so it is
followed by a newline. The main text body comes next, and must
also be followed by a newline, to make openssl happy."

  (insert pass)
  (newline)
  (insert text)
  (newline))

(defun openssl-crypt-in-input-region (action start end)
  "Replace the given region with the result of encrypting or or
decrypting it.

The ACTION arg must either be the symbol ENCRYPT or the symbol
DECRYPT.

The first line of the region will be used as the encryption
key. All following lines of the region will be the main text
body. Be warned: the last line should end with a newline, unless
you like it when software refuses to work properly."

  (unless (member action '(encrypt decrypt))
    (error "ACTION must either be the symbol `encrypt' or `decrypt'"))
  (save-excursion
    (apply 'call-process-region
           start end
           openssl-crypt-command
           t ; Delete the input region
           t ; Output goes in the current buffer
           nil ; Don't display the output incrementally
           openssl-crypt-cipher
           (if (eq action 'decrypt)
               ;; Add the -d option, to decrypt
               (cons "-d" openssl-crypt-args)
             openssl-crypt-args))))

(defun openssl-crypt (action pass text)
  "Encrypt or decrypt a string of text with a password.

The ACTION arg must either be the symbol ENCRYPT or the symbol
DECRYPT."

  (unless (member action '(encrypt decrypt))
    (error "ACTION must either be ENCRYPT or DECRYPT"))
  (save-excursion
    (with-temp-buffer
      ;; Fill temporary buffer with the input for openssl.
      (openssl-crypt-setup-input pass text)
      ;; Replace the input in the temp buffer with the encrypted
      ;; output.
      (openssl-crypt-in-input-region action
                                     (point-min)
                                     (point-max))
      (buffer-string))))

(defun openssl-crypt-region (action start end pass
                                    &optional replace-p)
  "Return the result of encrypting or decrypting the given region
as a string.

The ACTION arg must either be the symbol ENCRYPT or the symbol
DECRYPT.

If REPLACE-P is non-nil, also replace the input region with its
encrypted/decrypted result."

  (let* ((text (buffer-substring start end))
         (result (openssl-crypt action pass text)))
    (when replace-p
      (delete-region start end)
      (insert result))
    result))


;;; Interactive Commands

(defun openssl-crypt-encrypt-region (start end replace-p)
  "Prompt for a password, and encrypt the given region.

If the universal prefix arg is given, or REPLACE-P is non-nil,
replace the region with the encrypted data; otherwise display it
in a temporary buffer."

  (interactive "d\nm\nP")
  (unless (region-active-p)
    (error "Region not active"))
  (let* ((pass (read-passwd "Password: " t))
         (result (openssl-crypt-region 'encrypt start end
                                       pass replace-p)))
    (clear-string pass)
    (unless replace-p
      (with-output-to-temp-buffer "*openssl-crypt encrypted data*"
        (princ result)))))

(defun openssl-crypt-decrypt-region (start end replace-p)
  "Prompt for a password, and decrypt the given region.

If the universal prefix arg is given, or REPLACE-P is non-nil,
replace the region with the decrypted data; otherwise, display it
in a temporary buffer."

  (interactive "m\nd\nP")
  (unless (region-active-p)
    (error "Region not active"))
  (let* ((pass (read-passwd "Password: "))
         (result (openssl-crypt-region 'decrypt start end
                                       pass replace-p)))
    (clear-string pass)
    (unless replace-p
      (with-output-to-temp-buffer "*openssl-crypt decrypted data*"
        (princ result)))))

(defun openssl-crypt-encrypt-string (insert-p)
  "Prompt for a password and a string, and encrypt the string.

If the universal prefix arg is given, or INSERT-P is non-nil,
insert the encrypted data into the current buffer; otherwise,
display it in a temporary buffer."

  (interactive "P")
  (let* ((pass (read-passwd "Password: " t))
         (text (read-string "String: "))
         (result (openssl-crypt 'encrypt pass text)))
    (clear-string pass)
    (if insert-p
        (insert result)
      (save-excursion
        (with-output-to-temp-buffer "*openssl-crypt encrypted data*"
          (princ result))))))

(defun openssl-crypt-decrypt-string (insert-p)
  "Prompt for a password and a string, and decrypt the string.

If the universal prefix arg is given, or INSERT-P is non-nil,
insert the decrypted data into the current buffer; otherwise,
display it in a temporary buffer."

  (interactive "P")
  (let* ((pass (read-passwd "Password: "))
         (text (read-string "String: "))
         (result (openssl-crypt 'decrypt pass text)))
    (clear-string pass)
    (if insert-p
        (insert result)
      (with-output-to-temp-buffer "*openssl-crypt decrypted data*"
        (princ result)))))

(provide 'openssl-crypt)
