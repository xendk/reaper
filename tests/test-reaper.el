;;; test-reaper.el --- Tests for reaper              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'buttercup)
(require 'reaper)

(describe "user supplied date parsing"
  (before-each
    ;; Fix tests in time.
    (spy-on 'current-time :and-return-value (date-to-time "2018-07-11T12:33:05Z"))
    (setq reaper-date "2020-11-14"))
  (it "returns current date for an empty input"
    (expect (reaper--parse-date-string "")
            :to-equal
            "2018-07-11"))
  (it "parses a full date"
    (expect (reaper--parse-date-string "2010-08-26")
            :to-equal
            "2010-08-26"))
  (it "rejects future dates"
    (expect (reaper--parse-date-string "2018-07-11")
            :to-equal
            nil))
  (it "parses dot notation"
    (expect (reaper--parse-date-string "2010.08.26")
            :to-equal
            "2010-08-26"))
  (it "allows omitting year"
    ;; Note that it goes a year back because the date in the current year is in the future.
    (expect (reaper--parse-date-string "08.26")
            :to-equal
            "2017-08-26"))
  (it "allows omitting year and month"
    (expect (reaper--parse-date-string "10")
            :to-equal
            "2018-07-10"))
  (it "goes a month back if current day is less than supplied"
    (expect (reaper--parse-date-string "20")
            :to-equal
            "2018-06-20"))
  (it "goes back that many days from reaper-date if given a negative number"
    (expect (reaper--parse-date-string "-3")
            :to-equal
            "2020-11-11"))
  (it "goes forward that many days from reaper-date if given a number prefixed with plus"
    (expect (reaper--parse-date-string "+3")
            :to-equal
            "2020-11-17"))
  (it "-<days> handles month rollover"
    (expect (reaper--parse-date-string "-30")
            :to-equal
            "2020-10-15"))
  (it "+<days> handles month rollover"
    (expect (reaper--parse-date-string "+30")
            :to-equal
            "2020-12-14")))

(describe "time to hours parsing"
  (it"parses 1:00"
    (expect (reaper--time-to-hours "1:00")
            :to-equal
            1.0)
    )
  (it "parses 2:30"
    (expect (reaper--time-to-hours "2:30")
            :to-equal
            2.5))
  (it "parses 50"
    (expect (reaper--time-to-hours "45")
            :to-equal
            0.75))
  (it "parses 75"
    (expect (reaper--time-to-hours "75")
            :to-equal
            1.25))
  (it "rejects empty string"
    (expect (reaper--time-to-hours "")
            :to-equal
            nil))
  (it "rejects 1:0:0"
    (expect (reaper--time-to-hours "1:0:0")
            :to-equal
            nil))
  (it "rejects a:38"
    (expect (reaper--time-to-hours "a:38")
            :to-equal
            nil))
  (it "rejects a"
    (expect (reaper--time-to-hours "a")
            :to-equal
            nil))
  )

(describe "reaper--get-user-id"
  (before-each
    (spy-on 'reaper-api
            :and-return-value '((id . 123)
                                (first_name . "First")
                                (last_name . "Name")
                                (email . "user@example.com")
                                (telephone . ""))))

  (it "returns the proper id"
    (expect (reaper--get-user-id) :to-be 123))

  (describe "on invalid respone"
    (before-each
      (spy-on 'reaper-api
              :and-return-value '((noid . 123))))
    (it "will throw an error"
      (expect (reaper--get-user-id) :to-throw))))

(provide 'test-reaper)
;;; test-reaper.el ends here
