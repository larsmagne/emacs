;;; url-cache.el --- Uniform Resource Locator retrieval tool  -*- lexical-binding: t; -*-

;; Copyright (C) 1996-1999, 2004-2022 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'url-parse)
(require 'url-util)
(require 'url)                        ;E.g. for url-configuration-directory.

(defcustom url-cache-directory
  (expand-file-name "cache" url-configuration-directory)
  "The directory where cache files should be stored."
  :type 'directory
  :group 'url-file)

(defcustom url-cache-expire-time 3600
  "Default maximum time in seconds before cache files expire.
Used by the function `url-cache-expired'.  Also see
`url-cache-respect-headers'."
  :version "24.1"
  :type 'natnum
  :group 'url-group)

(defcustom url-cache-respect-headers t
  "If non-nil, respect Expires and Cache-Control HTTP headers.
This will make cache expiry slower as each file will have to be
inspected, while `url-cache-expire-time' just looks at
modification times.

If there are no caching headers in the documents,
`url-cache-expire-time' will be respected."
  :version "29.1"
  :type 'bool
  :group 'url-group)

;; Cache manager
(defun url-cache-file-writable-p (file)
  "Follows the documentation of `file-writable-p', unlike `file-writable-p'."
  (and (file-writable-p file)
       (if (file-exists-p file)
           (not (file-directory-p file))
         (file-directory-p (file-name-directory file)))))

(defun url-cache-prepare (file)
  "Make it possible to cache data in FILE.
Creates any necessary parent directories, deleting any non-directory files
that would stop this.  Returns nil if parent directories can not be
created.  If FILE already exists as a non-directory, it changes
permissions of FILE or deletes FILE to make it possible to write a new
version of FILE.  Returns nil if this can not be done, or if FILE already
exists as a directory.  Otherwise, returns t, indicating that
FILE can be created or overwritten."
  (cond
   ((url-cache-file-writable-p file)
    t)
   ((file-directory-p file)
    nil)
   (t
    (condition-case ()
	(or (make-directory (file-name-directory file) t) t)
      (error nil)))))

;;;###autoload
(defun url-store-in-cache (&optional buff)
  "Store buffer BUFF in the cache."
    (with-current-buffer (get-buffer (or buff (current-buffer)))
      (let ((fname (url-cache-create-filename (url-view-url t))))
        (if (url-cache-prepare fname)
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) fname nil 5))))))

(defun url-fetch-from-cache (url)
  "Fetch URL from cache and return a buffer with the content."
  (with-current-buffer (generate-new-buffer " *temp*")
    (url-cache-extract (url-cache-create-filename url))
    (current-buffer)))

;;;###autoload
(defun url-is-cached (url)
  "Return non-nil if the URL is cached.
The actual return value is the last modification time of the cache file."
  (let* ((fname (url-cache-create-filename url))
	 (attribs (file-attributes fname)))
    (and fname
	 (file-exists-p fname)
	 (not (eq (file-attribute-type attribs) t))
	 (file-attribute-modification-time attribs))))

(defun url-cache-create-filename-human-readable (url)
  "Return a filename in the local cache for URL."
  (if url
      (let* ((urlobj (url-generic-parse-url url))
	     (protocol (url-type urlobj))
	     (hostname (url-host urlobj))
	     (host-components
	      (cons
	       (user-real-login-name)
	       (cons (or protocol "file")
		     (reverse (split-string (or hostname "localhost")
					    "\\.")))))
	     (fname    (url-filename urlobj)))
	(if (and fname (/= (length fname) 0) (= (aref fname 0) ?/))
	    (setq fname (substring fname 1 nil)))
	(if fname
	    (let ((slash nil))
	      (setq fname
		    (mapconcat
                     (lambda (x)
                       (cond
                        ((and (= ?/ x) slash)
                         (setq slash nil)
                         "%2F")
                        ((= ?/ x)
                         (setq slash t)
                         "/")
                        (t
                         (setq slash nil)
                         (char-to-string x)))) fname ""))))

	(setq fname (and fname
			 (mapconcat
                          (lambda (x)
                            (if (= x ?~) "" (char-to-string x)))
			  fname ""))
	      fname (cond
		     ((null fname) nil)
		     ((or (string= "" fname) (string= "/" fname))
		      url-directory-index-file)
		     ((= (string-to-char fname) ?/)
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			(substring fname 1 nil)))
		     (t
		      (if (string= (substring fname -1 nil) "/")
			  (concat fname url-directory-index-file)
			fname))))
	(and fname
	     (expand-file-name fname
			       (expand-file-name
				(mapconcat 'identity host-components "/")
				url-cache-directory))))))

(defun url-cache-create-filename-using-md5 (url)
  "Create a cached filename using MD5.
Very fast if you have an `md5' primitive function, suitably fast otherwise."
  (if url
      (let* ((checksum (md5 url))
	     (urlobj (url-generic-parse-url url))
	     (protocol (url-type urlobj))
	     (hostname (url-host urlobj))
	     (host-components
	      (cons
	       (user-real-login-name)
	       (cons (or protocol "file")
		     (nreverse
		      (delq nil
			    (split-string (or hostname "localhost")
					  "\\."))))))
	     (fname    (url-filename urlobj)))
	(and fname
	     (expand-file-name checksum
			       (expand-file-name
				(mapconcat 'identity host-components "/")
				url-cache-directory))))))

(defcustom url-cache-creation-function 'url-cache-create-filename-using-md5
  "What function to use to create a cached filename."
  :type '(choice (const :tag "MD5 of filename (low collision rate)"
			:value url-cache-create-filename-using-md5)
		 (const :tag "Human readable filenames (higher collision rate)"
			:value url-cache-create-filename-human-readable)
		 (function :tag "Other"))
  :group 'url-cache)

(defun url-cache-create-filename (url)
  (funcall url-cache-creation-function
           ;; We need to parse+recreate in order to remove the default port
           ;; if it has been specified: e.g. http://www.example.com:80 will
           ;; be transcoded as http://www.example.com
           (url-recreate-url
            (if (url-p url) url
              (url-generic-parse-url url)))))

;;;###autoload
(defun url-cache-extract (fnam)
  "Extract FNAM from the local disk cache."
  (erase-buffer)
  (set-buffer-multibyte nil)
  (insert-file-contents-literally fnam))

(defun url-cache-expired (url &optional expire-time)
  "Return non-nil if a cached URL is older than EXPIRE-TIME seconds.
The default value of EXPIRE-TIME is `url-cache-expire-time'.
If `url-standalone-mode' is non-nil, cached items never expire."
  (let ((file (url-cache-create-filename url)))
    (if url-standalone-mode
        (not (file-exists-p file))
      (let ((status (if url-cache-respect-headers
                        (url-cache--http-expiry-status file)
                      'unknown)))
        ;; If the file didn't say what the status was, or we're just
        ;; using timestamps, check the timestamp.
        (if (not (eq status 'unknown))
            status
          (let ((cache-time (url-is-cached url)))
            (or (not cache-time)
	        (time-less-p
	         (time-add
	          cache-time
	          (or expire-time url-cache-expire-time))
	         nil))))))))

(defun url-cache-prune-cache (&optional directory)
  "Remove all expired files from the cache.
`url-cache-expire-time' says how old a file has to be to be
considered \"expired\"."
  (let ((now (current-time))
	(total-files 0)
	(deleted-files 0))
    (setq directory (or directory url-cache-directory))
    (when (file-exists-p directory)
      (dolist (file (directory-files directory t))
	(unless (member (file-name-nondirectory file) '("." ".."))
	  (setq total-files (1+ total-files))
	  (if (file-directory-p file)
	      (when (url-cache-prune-cache file)
	        (setq deleted-files (1+ deleted-files)))
            (let ((status (if url-cache-respect-headers
                              (url-cache--http-expiry-status file)
                            'unknown)))
              (when (or (eq status t)
                        (and (eq status 'unknown)
	                     (time-less-p
	                      (time-add
	                       (file-attribute-modification-time
                                (file-attributes file))
	                       url-cache-expire-time)
	                      now)))
	        (delete-file file)
	        (setq deleted-files (1+ deleted-files)))))))
      (if (< deleted-files total-files)
	  nil
	(delete-directory directory)
	t))))

(defun url-cache--http-expiry-status (file)
  "Say whether FILE is expired based on the HTTP headers in FILE.
There are three possible values: nil, t and `unknown'.  The
latter if there are no cache control headers in FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((atts (file-attributes file))
           (size (file-attribute-size atts))
           (case-fold-search t)
           (start 0))
      ;; Get the entire header.
      (while (and (or (zerop start)
                      (not (re-search-forward "^$" nil t)))
                  (< start size))
        (goto-char (point-max))
        (insert-file-contents file nil 0 (min 1024 size))
        (setq start (+ start 1024)))
      (goto-char (point-min))
      (cond
       ((not (re-search-forward "^$" nil t))
        'unknown)
       ((save-excursion
          (re-search-backward "^cache-control: \\(.*\\)" nil t))
        (url-cache--cache-control-expired-p
         (match-string 1) (file-attribute-modification-time atts)))
       ((re-search-backward "^expires: \\(.*\\)" nil t)
        (url-cache--expires-expired-p (match-string 1)))
       (t 'unknown)))))

(defun url-cache--cache-control-expired-p (string file-time)
  ;; The string is something like "public, max-age=31536000".
  (seq-some
   (lambda (elem)
     (and (string-match "max-age=\\([0-9]+\\)" elem)
          (< (+ (string-to-number (match-string 1 elem))
                (time-convert file-time 'integer))
             (time-convert (current-time) 'integer))))
   (split-string string ",")))

(defun url-cache--expires-expired-p (date)
  (time-less-p (encode-time (parse-time-string date))
               (current-time)))

(provide 'url-cache)

;;; url-cache.el ends here
