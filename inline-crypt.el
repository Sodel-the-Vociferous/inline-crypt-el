;;; inline-crypt.el --- Simple inline encryption via openssl
;;; Author: Daniel Ralston <Wubbulous@gmail.com>
;;; Version: 0.1.4
;;; Url: https://github.com/Sodel-the-Vociferous/inline-crypt-el
;;; Keywords: crypt

;;; Commentary:

;; This package may be useful if you’d just like to encrypt parts of
;; files or buffers, and to have encrypted output in the same line as
;; other unencrypted text.

;; This package relies on the openssl command-line utility. If openssl
;; isn't in your PATH, customize the `inline-crypt-openssl-command'
;; variable to point to it.

;; The four interactive commands that will interest you are
;; `inline-crypt-encrypt-region', `inline-crypt-decrypt-region',
;; `inline-crypt-encrypt-string', and `inline-crypt-decrypt-string'.

;;; License:

;; Copyright (C) 2013, Daniel Ralston <Wubbulous@gmail.com>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 2 as
;; published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(provide 'inline-crypt)

(defconst inline-crypt-openssl-args
  '("-a" ; Use base64 encoding
    "-salt" ; Salt, for flavour and security
    "-pass" "stdin")) ; Use the first line of stdin as the password

(defcustom inline-crypt-openssl-command "openssl"
  "The base openssl command to run when encrypting or decrypting.")

(defcustom inline-crypt-cipher "aes-256-cbc"
  "The default cipher to use when encrypting or decrypting.")


(defun inline-crypt-setup-openssl-input (pass text)
  "Insert the password and main text body into the current
buffer before the point, for input into openssl.

The password must be all by itself on the first line, so it is
followed by a newline. The main text body comes next, and must
also end with a newline, to make openssl happy."

  (insert pass)
  (newline)
  (insert text)
  (newline))

(defun inline-crypt-replacing-input-region (action start end)
  "Replace the given region with the result of encrypting or or
decrypting it.

ACTION must either be the symbol ENCRYPT or the symbol DECRYPT.

The first line of the region will be used as the encryption
key. All following lines of the region will be the main text
body. Be warned: the last line should end with a newline, unless
you like it when software refuses to work properly."

  (unless (member action '(encrypt decrypt))
    (error "ACTION must either be the symbol `encrypt' or `decrypt'"))
  (save-excursion
    (apply 'call-process-region
           start end
           inline-crypt-openssl-command
           t ; Delete the input region
           t ; Output goes in the current buffer
           nil ; Don't display the output incrementally
           inline-crypt-cipher
           (if (eq action 'decrypt)
               ;; Add the -d option, to decrypt
               (cons "-d" inline-crypt-openssl-args)
             inline-crypt-openssl-args))
    ;; Remove the final newline, which was added by openssl
    (backward-delete-char 1)))

(defun inline-crypt (action pass text)
  "Encrypt or decrypt a string of text with a password.

ACTION must either be the symbol ENCRYPT or the symbol DECRYPT."

  (unless (member action '(encrypt decrypt))
    (error "ACTION must either be ENCRYPT or DECRYPT"))
  (save-excursion
    (with-temp-buffer
      ;; Fill temporary buffer with the input for openssl.
      (inline-crypt-setup-openssl-input pass text)
      ;; Replace the input in the temp buffer with the encrypted
      ;; output.
      (inline-crypt-replacing-input-region action
                                           (point-min)
                                           (point-max))
      (buffer-string))))

(defun inline-crypt-region (action start end pass
                                   &optional replace-p)
  "Return the result of encrypting or decrypting the given region,
as a string.

ACTION must either be the symbol ENCRYPT or the symbol
DECRYPT. If REPLACE-P is non-nil, also replace the input region
with its encrypted/decrypted result."

  (let* ((text (buffer-substring start end))
         (result (inline-crypt action pass text)))
    (when replace-p
      (delete-region start end)
      (insert result))
    result))


;;; Interactive Commands

(defun inline-crypt-encrypt-region (start end pass
                                          &optional replace-p)
  "Prompt for a password, and encrypt the given region.

If the universal prefix arg is given, or REPLACE-P is non-nil,
replace the region with the encrypted data; otherwise display it
in a temporary buffer."

  (interactive "d\nm\ni\nP")
  (unless (region-active-p)
    (error "Region not active"))
  (let* ((pass (or pass (read-passwd "Encryption Password: " t)))
         (result (inline-crypt-region 'encrypt start end
                                      pass replace-p)))
    (clear-string pass)
    (unless replace-p
      (with-output-to-temp-buffer "*inline-crypt encrypted data*"
        (princ result)))))

(defun inline-crypt-decrypt-region (start end pass
                                          &optional replace-p)
  "Prompt for a password, and decrypt the given region.

If the universal prefix arg is given, or REPLACE-P is non-nil,
replace the region with the decrypted data; otherwise, display it
in a temporary buffer."

  (interactive "m\nd\ni\nP")
  (unless (region-active-p)
    (error "Region not active"))
  (let* ((pass (or pass (read-passwd "Decryption Password: ")))
         (result (inline-crypt-region 'decrypt start end
                                      pass replace-p)))
    (clear-string pass)
    (unless replace-p
      (with-output-to-temp-buffer "*inline-crypt decrypted data*"
        (princ result)))))

(defun inline-crypt-encrypt-string (insert-p)
  "Prompt for a password and a string, and encrypt the string.

If the universal prefix arg is given, or INSERT-P is non-nil,
insert the encrypted data into the current buffer; otherwise,
display it in a temporary buffer."

  (interactive "P")
  (let* ((pass (read-passwd "Encryption Password: " t))
         (text (read-string "String to Encrypt: "))
         (result (inline-crypt 'encrypt pass text)))
    (clear-string pass)
    (if insert-p
        (insert result)
      (save-excursion
        (with-output-to-temp-buffer "*inline-crypt encrypted data*"
          (princ result))))))

(defun inline-crypt-decrypt-string (insert-p)
  "Prompt for a password and a string, and decrypt the string.

If the universal prefix arg is given, or INSERT-P is non-nil,
insert the decrypted data into the current buffer; otherwise,
display it in a temporary buffer."

  (interactive "P")
  (let* ((pass (read-passwd "Decryption Password: "))
         (text (read-string "String to Decrypt: "))
         (result (inline-crypt 'decrypt pass text)))
    (clear-string pass)
    (if insert-p
        (insert result)
      (with-output-to-temp-buffer "*inline-crypt decrypted data*"
        (princ result)))))

;;; Create a submenu under Tools.
;; Moulph. This is uuuugly.

(easy-menu-add-item
 global-map '("menu-bar" "tools")
 (easy-menu-create-menu
  "inline-crypt"
  `(["Decrypt Region" inline-crypt-decrypt-region]
    ["Decrypt and Replace Region"
     ;; A function to simulate interactively passing the prefix arg to
     ;; the proper function.
     ,(lambda ()
        (interactive)
        (let ((current-prefix-arg t))
          (call-interactively 'inline-crypt-decrypt-region)))]
    ["Encrypt Region" inline-crypt-encrypt-region]
    ["Encrypt and Replace Region"
     ;; A function to simulate interactively passing the prefix arg to
     ;; the proper function.
     ,(lambda ()
        (interactive)
        (let ((current-prefix-arg t))
          (call-interactively 'inline-crypt-encrypt-region)))])))

;;; inline-crypt.el ends here
