(defpackage Chœrogryllum
  (:nicknames Choerogryllum)
  (:use :cl :oliphaunt)
  (:export
   #:decode*-universal-time
   #:encode*-universal-time
   #:day-of-week*
   #:holiday-on
   #:date-string
   #:month*
   #:cal-month.html
   #:cal-month
   #:cal-year))

(in-package :Chœrogryllum)



(defun holiday-on (year month day)
  "Returns the name of any holiday on YEAR, MONTH, DAY.

YEAR, MONTH, and DAY are the integral values of the Chœrogryllym year,
month, and day.

If there is no holiday, but there is a full moon (any moon), that may
be reported instead.

@cindex Chœrogryllum Holidays

@subsection Chœrogryllum Holiday

The following holidays are recognized and reported:

@table @b

@item Trimestus
occurs when all three moons are full. Since the
months are evenly matched to the phases of The Moon, this will always
occur on the 15 day of some month. This is a major festival day.
@item Hallowe'en
occurs on the 30 day of Procavia (month 10).
@item Hallowsday
occurs on the 1st day of Dendrohyrax (month 11).
@item Easter
occurs on Lightningsday some time between Inunguis,
Manatus, or Hydrodamalis, (months 3-5) based on the phase of The Other
Moon.
@item Christmas
occurs on the 25 day of Tethytheria (month 12).
@item Christmas Eve
occurs on the 24 day of Tethytheria (month 12).
@item Parents' Day
occurs on the 13 day of Hydrodamalis (month 5). It takes the place of
both Mothers' Day and Fathers' Day on Earth.
@item The Winter Solstice
occurs on the 21 day of Sirenia (month 1).
@item The Spring Equinox
occurs on the 21 day of Manatus (month 4).
@item The Summer Solstice
occurs on the 21 day of Pygmaeus (month 7).
@item The Autumn Equinox
occurs on the 21 day of Procavia (month 10).
@item Fawkesday
occurs on the 5 day of Dendrohyrax (month 11).
@item New Year's Day
occurs on the first day of Sirenia (month 1).
@item The Summer Arts Festival
runs from the 17th to the 20th day of Pygmaeus, except on Blanksday.
@item Duomestus
occurs whenever the Other Moon and Pink Moon are both full

@end table
"
  (multiple-value-bind
        (_sec _min _hour _day _month _year
              weekday other-month-day pink-month-day)
      (decode*-universal-time (encode*-universal-time 0 0 9 day month year))
    (declare (ignore _sec _min _hour _day _month _year))
    (cond
      ((and (= 15 day)
            (= 36 other-month-day)
            (= 26 pink-month-day)) "Trimestus")
      ((and (= 30 day) (= 10 month)) "Hallowe'en")
      ((and (= 1 day) (= 11 month)) "Hallowsday")
      ((and (or (= 3 month) (= 4 month) (= 5 month))
            (= 0 weekday) (< other-month-day 10)) "Easter")
      ((and (= 25 day) (= 12 month)) "Christmas")
      ((and (= 24 day) (= 12 month)) "Christmas Eve")
      ((and (= 13 day) (= 5 month)) "Parents' Day")
      ((and (= day 21) (= 1 month)) "Winter Solstice")
      ((and (= day 21) (= 4 month)) "Spring Equinox")
      ((and (= day 21) (= 7 month)) "Summer Solstice")
      ((and (= day 21) (= 10 month)) "Autumn Equinox")
      ((and (= 5 day) (= 11 month)) "Fawkesday")
      ((= 1 day month) "New Year's Day")
      ((and (= 30 day) (= 12 month)) "New Year's Eve")
      ((and (<= 17 day 20)
            (= 7 month) (/= 8 weekday)) "Summer Arts Festival")
      ((and (= 36 other-month-day)
            (= 26 pink-month-day)) "Duomestus")
      ((= 36 other-month-day) "Full Other Moon")
      ((= 26 pink-month-day) "Full Pink Moon")
      ((= 15 day) "Full Moon")
      (t nil))))

(defun exponent-digit (number)
  "Returns the digit NUMBER in exponent (superscript) character form"
  (check-type number (integer 0 10))
  (coerce (list (elt "⁰¹²³⁴⁵⁶⁷⁸⁹†" number)) 'string))

(defun cal-month/print-holiday-footnotes (year month holidays stream)
  (when holidays
    (loop for holiday in (nreverse holidays)
       for i from 1
       do (format stream "~% *~a: ~a on ~a" (exponent-digit i)
                  (holiday-on year month holiday)
                  (date-string (encode*-universal-time 0 0 9 holiday month year))))
    (terpri stream)))

(defun first-weekday-of-month (year month)
  "Returns the weekday number (0-8) of the first day of MONTH in YEAR."
  (nth-value 6 (decode*-universal-time (encode*-universal-time 0 0 9 1 month year))))

(defun cal-month-header (year month stream)
  "Prints a header for a calendar of MONTH in YEAR to STREAM."
  (declare (ignore year))
  (format stream "~45@<~:@r. ~a  ~;---------------------------------------------~>

~{~a.~^ ~}~%"
          month (month* month)
          (mapcar (rcurry #'day-of-week* :form :abbrev) (range 0 8))))

(defun cal-month-header.html (year month stream)
  "Writes an HTML header for a calendar of MONTH in YEAR to STREAM."
  (declare (ignore year))
  (format stream "<TR><TH COLSPAN='9'>~:@r. ~a </TH></TR>
<TR> ~{<TH>~a.</TH>~^ ~} </TR>~%"
          month (month* month)
          (mapcar (rcurry #'day-of-week* :form :abbrev) (range 0 8))))

(defun cal-month (year month)
  "Pretty-prints a one-month mini-calendar."
  (let ((first-weekday-of-month (first-weekday-of-month year month)))
    (with-output-to-string (s)
      (cal-month-header year month s)
      (loop for pad-day below first-weekday-of-month
         do (princ "     " s))
      (let (holidays)
        (loop for day from 1 upto 30
           for holiday = (holiday-on year month day)
           do (format s " ~2d~2a" day (if holiday
                                          (progn
                                            (push day holidays)
                                            (concatenate 'string "*" (exponent-digit (length holidays))))
                                          "  "))
           when (zerop (mod (+ day first-weekday-of-month) 9))
           do (terpri s))
        (terpri s)
        (cal-month/print-holiday-footnotes year month holidays s)
        (princ "---------------------------------------------" s)
        (terpri s)))))

(defun this-year ()
  (nth-value 5 (decode*-universal-time)))

(defun this-month ()
  (nth-value 4 (decode*-universal-time)))

(defun cal-month.html (&optional (year (this-year)) (month (this-month)))
  "Pretty-prints a one-month mini-calendar."
  (let ((first-weekday-of-month (first-weekday-of-month year month)))
    (with-output-to-string (s)
      (princ "<TABLE><TR><TH COLSPAN=9>" s)

      (cal-month-header.html year month s)
      (princ "</TH></TR><TR>" s)
      (loop for pad-day below first-weekday-of-month
         do (princ " <TD></TD> " s))
      (let (holidays)
        (loop for day from 1 upto 30
           for holiday = (holiday-on year month day)
           do (if holiday
                  (format s "<TD><ABBR TITLE=\"~a\"> ~2d </ABBR> </TD>"
                          holiday day)
                  (format s "<TD> ~2d </TD>" day))
           when (zerop (mod (+ day first-weekday-of-month) 9))
           do (princ "</TR><TR>" s))
        (terpri s)
        (princ "</TABLE>" s)
        (terpri s)))))

(defun cal-year (year)
  (with-output-to-string (s)
    (dotimes (month 12)
      (princ (cal-month year (1+ month)) s))))

(defun date-string (time &key (form :long))
  "Returns the pretty-printed Chœrogryllum date string describing Universal time TIME."
  (multiple-value-bind  (sec min hour day month year weekday)
      (decode*-universal-time time)
    (declare (ignore hour min sec))
    (ecase form
      (:long (format nil "~a, the ~:r of ~a, ~d"
                     (day-of-week* weekday) day (month* month) year))
      (:abbrev (format nil "~a ~2,'0d ~5a ~d"
                       (day-of-week* weekday :form :abbrev) day (month* month :form :abbrev) year)))))

(defun encode*-universal-time (sec min hour day month year)
  "Encodes a Chœrogryllum date & time into a Universal Time."
  (check-type year (integer 0 *))
  (check-type month (integer 1 12))
  (check-type day (integer 1 30))
  (check-type hour (integer 0 17))
  (check-type min (integer 0 59))
  (check-type sec (integer 0 59))
  (round (+
          (* (+ year 10) 60 60 24 270)
          (* (1- month) (/ (* 60 60 24 270) 12))
          (* (1- day) (/ (* 60 60 24 270) (* 12 30)))
          (* hour (/ (* 60 60 24 270) (* 12 30 18)))
          (* min (/ (* 60 60 24 270) (* 12 30 18 60)))
          (* sec (/ (* 60 60 24 270) (* 12 30 18 60 60))))))

(defun decode*-universal-time (&optional (time (get-universal-time)))
  "Returns multiple values with date and time decoded.

Returns: 
\(sec min hour day month year weekday other-month-day pink-month-day julian)
"
  (let* ((year (- (floor time (* 60 60 24 270)) 10))
         (month (1+ (floor (mod time (* 60 60 24 270)) (/ (* 60 60 24 270) 12))))
         (day (1+ (floor (mod time (/ (* 60 60 24 270) 12)) (/ (* 60 60 24 270) (* 12 30)))))
         (hour (floor (mod time (/ (* 60 60 24 270) (* 12 30))) (/ (* 60 60 24 270) (* 12 30 18))))
         (min  (floor (mod time (/ (* 60 60 24 270) (* 12 30 18))) (/ (* 60 60 24 270) (* 12 30 18 60))))
         (sec  (floor (mod time (/ (* 60 60 24 270) (* 12 30 18 60))) (/ (* 60 60 24 270) (* 12 30 18 60 60))))
         (julian (mod (+ day (* 30 month) (* 270 year)) 270))
         (weekday (mod (+ 3 julian) 9))
         (other-month-day (1+ (mod (+ 19 18 julian) 71)))
         (pink-month-day (1+ (mod (+ 11 18 julian) 53))))
    (values (the (mod 60) sec)
            (the (mod 60) min)
            (the (mod 18) hour)
            (the (integer 1 30) day)
            (the (mod 12) month)
            (the integer year)
            (the (mod 9) weekday)
            (the (integer 1 72) other-month-day)
            (the (integer 1 54) pink-month-day)
            (the (mod 270) julian))))

(defun day-of-week* (i &key (form :long))
  (elt (ecase form
         (:long (list "Lightningday" "Spotsday" "Starsday"
                      "Notesday" "Sparklesday" "Moosday"
                      "Heartsday" "Floralday" "Blanksday"))
         (:abbrev (list "Ltn" "Spt" "Str" "Not" "Spk" "Moo" "Hrt" "Flr" "Bnk")))
       i))

(defun month* (i &key (form :long))
  (elt (ecase form
         (:long (list "Sirenia" "Dugon" "Inunguis"
                      "Manatus" "Hydrodamalis" "Senecalensis"
                      "Pygmaeus" "Luxodonta" "Elephas"
                      "Procavia" "Dendrohyrax" "Tethytheria"))
         (:abbrev (list "Sir" "Dug" "Inu"
                        "Man" "Hydr" "Sen"
                        "Pyg" "Lux" "Eleph"
                        "Pro" "Den" "Teth")))
       (1- i)))
