;;; url-cache-tests.el --- Test suite for url-cache.  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

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

(require 'ert)
(require 'ert-x)
(require 'url-cache)

(ert-deftest test-cache-control ()
  (should-not
   (url-cache--cache-control-expired-p "public, max-age=86400" (current-time)))
  (should-not
   (url-cache--cache-control-expired-p "public, max-age=10000086400"
                                       (- (time-convert (current-time) 'integer)
                                          86410)))
  (should
   (url-cache--cache-control-expired-p "public, max-age=86400"
                                       (- (time-convert (current-time) 'integer)
                                          86410)))
  (should-not
   (url-cache--cache-control-expired-p "public, public " (current-time))))

(ert-deftest test-expires ()
  (should (url-cache--expires-expired-p "Wed, 21 Oct 2015 07:28:00 GMT"))
  ;; REMEMBER TO UPDATE THIS IN 2215!!!
  (should-not (url-cache--expires-expired-p "Wed, 21 Oct 2215 07:28:00 GMT")))

(ert-deftest text-http-expiry-file ()
  (should
   (eq (url-cache--http-expiry-status (ert-resource-file "expired.bin"))
       t))
  (should-not
   (eq (url-cache--http-expiry-status (ert-resourcpe-file "not-expired.bin"))
       t)))

;;; url-cache-tests.el ends here
