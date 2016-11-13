(ql:quickload '(ol-utils drakma cl-csv parse-number eazy-gnuplot))
(in-package :ol-user)

(defpar *stock-csv-data*
        (cl-csv:read-csv
         (drakma:http-request "http://chart.finance.yahoo.com/table.csv?s=SPY&a=0&b=1&c=2000&d=11&e=31&f=2010&g=d&ignore=.csv"
                              :want-stream t :close t)))

(subseq *stock-csv-data* 0 10)

(defclass/f stock ()
  (stock-ticker
   stock-date
   stock-open stock-high stock-low stock-close stock-volume))

(defmethod print-object ((stock stock) stream)
  (print-unreadable-object (stock stream :type t)
    (with-accessors ((stock-ticker stock-ticker)
                     (stock-date stock-date))
        stock
      (format stream "~a LEN: ~a" stock-ticker (length stock-date))))
  stock)

(defun csv->stock (ticker stock-csv-data)
  (iter (for event in (rest stock-csv-data)) ; remove header
        (collect (elt event 0) into stock-date result-type vector)
        (collect (parse-number:parse-number (elt event 1)) into stock-open result-type vector)
        (collect (parse-number:parse-number (elt event 2)) into stock-high result-type vector)
        (collect (parse-number:parse-number (elt event 3)) into stock-low result-type vector)
        (collect (parse-number:parse-number (elt event 4)) into stock-close result-type vector)
        (collect (parse-number:parse-number (elt event 5)) into stock-volume result-type vector)
        (finally (return (make-instance 'stock
                                        :stock-ticker ticker
                                        :stock-date stock-date
                                        :stock-open stock-open
                                        :stock-high stock-high
                                        :stock-low stock-low
                                        :stock-close stock-close
                                        :stock-volume stock-volume)))))

(defpar *current-stock* (csv->stock "SPY" *stock-csv-data*))

(defclass/f stock-subseq (stock)
  (parent start end))

(defun stock-subseq (stock start &optional end)
  ;; if end is larger than sequence length, just go to sequence end
  (let1 (len (length (stock-date stock)))
    (unless (or (not end) (<= end len))
      (setf end len)))
  (make-instance 'stock-subseq :parent stock :start start :end end))

(defmethod stock-ticker ((stock-subseq stock-subseq))
  (stock-ticker (parent stock-subseq)))

;; bind multi copies the definition for stock-date for the other slots
(bind-multi ((stock-date stock-date stock-open stock-high stock-low stock-close stock-volume))
  (defmethod stock-date ((stock-subseq stock-subseq))
    (subseq (stock-date (parent stock-subseq)) (start stock-subseq) (end stock-subseq))))

(defclass/f order ()
  (order-stock order-type order-start order-end order-volume))

(defun order-create (stock &optional (order-type :long))
  (make-instance 'order
                 :order-stock stock :order-type order-type
                 :order-start nil :order-end nil :order-volume nil))

(defmethod print-object ((object order) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (order-type order-end order-start order-volume)
        object
      (if order-end
          (format stream "~A Duration: ~A Volume: ~A" order-type (- order-start order-end) order-volume)
          (format stream "~A Start: ~A Volume: ~A" order-type order-start order-volume))))
  object)

(defmethod order-duration ((order order))
  (with-slots (order-start order-end) order
    (if order-end
        (abs (- order-start order-end))
        0)))

(defvar *current-date* 0)
(defpar *transaction-cost-factor* 0.0025)

(defmethod order-open ((order order) volume)
  ;; check if volume is available for long
  (when (and (eql (order-type order) :long)
             (< (aref (stock-volume (order-stock order)) *current-date*) volume))
    ;; todo signal something
    (return-from order-open nil))
  (setf (order-start order) *current-date*
        (order-volume order) volume)
  ;; buy/sell at opening time
  (let ((amount (* (if (eql (order-type order) :long) -1 1)
                   volume (aref (stock-open (order-stock order)) *current-date*))))
    ;; pay transaction fee
    (- amount (* *transaction-cost-factor* amount))))

(defmethod order-close ((order order))
  ;; check if volume is available for short
  (when (and (not (eql (order-type order) :long))
             (< (aref (stock-volume (order-stock order)) *current-date*) (order-volume order)))
    ;; todo signal something
    (return-from order-close nil))
  (setf (order-end order) *current-date*)
  ;; buy/sell at closing time
  (let ((amount (* (if (eql (order-type order) :long) 1 -1)
                   (order-volume order) (aref (stock-close (order-stock order)) *current-date*))))
    ;; pay transaction fee
    (- amount (* *transaction-cost-factor* amount))))

(defmethod stock-available-volume ((stock stock) time amount)
  (min (aref (stock-volume stock) *current-date*)
       (floor amount
              (* (aref (if (eql time :open) (stock-open stock) (stock-close stock)) *current-date*)
                 (+ 1 *transaction-cost-factor*)))))

(defmethod stock-available-volume ((stock stock) (order order) amount)
  (assert (eql stock (order-stock order)))
  (stock-available-volume stock (if (eql (order-type order) :long) :open :close) amount))

(defclass/f trader ()
  (trader-stock
   (trader-balance :initarg :trader-balance
                   :initform 0
                   :accessor trader-balance)
   (current-order :initarg :current-order
                  :initform nil
                  :accessor current-order)))

(create-standard-print-object trader trader-stock trader-balance)

(defmethod busy-p ((trader trader))
  (current-order trader))

(defmethod short ((trader trader) part)
  ;; here we need any positive number. keep in mind that anything
  ;; significantly larger than 1 is probably rather stupid.
  (assert (<= 0 part))
  (unless (busy-p trader)
    (with-accessors ((current-order current-order)) trader
      (setf current-order (order-create (trader-stock trader) :short))
      (let* ((amount (* part (trader-balance trader)))
             ;; don't do stupid stuff like go over available volume
             (volume (stock-available-volume (trader-stock trader) :open amount))
             ;; see if we can afford it
             (new-balance (+ (trader-balance trader)
                             (order-open current-order volume))))
        (if (< new-balance 0)
            ;; abort
            (progn
              (setf current-order nil)
              nil)
            ;; update balance
            (progn
              (setf (trader-balance trader) new-balance)
              t))))))

(defmethod long ((trader trader) part)
  ;; for this type of trade, we need a number between 0 and 1
  (assert (<= 0 part 1))
  (unless (busy-p trader)
    (with-accessors ((current-order current-order)) trader
      (setf current-order (order-create (trader-stock trader) :long))
      (let* ((amount (* part (trader-balance trader)))
             ;; don't do stupid stuff like go over available volume
             (volume (stock-available-volume (trader-stock trader) :open amount))
             ;; see if we can afford it
             (new-balance (+ (trader-balance trader)
                             (order-open current-order volume))))
        (if (< new-balance 0)
            ;; abort
            (progn
              (setf current-order nil)
              nil)
            ;; update balance
            (progn
              (setf (trader-balance trader) new-balance)
              t))))))

(defmethod conclude ((trader trader) &optional final)
  (when (busy-p trader)
    (with-accessors ((current-order current-order)) trader
      (let ((amount (order-close current-order)))
        ;; possibly `amount' is nil, if not enough volume is available
        (when amount
          (let ((new-balance (+ (trader-balance trader) amount)))
            (if (and (< new-balance 0) (not final))
                ;; abort
                nil
                ;; unset order, update balance
                (progn
                  (setf current-order nil
                        (trader-balance trader) new-balance)
                  t))))))))

(defclass/f reporting-trader (trader)
  ((balance-report :accessor balance-report)
   (order-list :initform nil
               :accessor order-list)))

(defmethod initialize-instance :after ((reporting-trader reporting-trader) &key)
  ;; balance-report should be an array with length matching the stock history
  (let ((history-length (length (stock-date (trader-stock reporting-trader)))))
    (setf (balance-report reporting-trader)
          (make-array history-length :initial-element nil))
    ;; we initialise the last entry (first date) with initial balance
    (setf (aref (balance-report reporting-trader) (- history-length 1))
          (trader-balance reporting-trader))))

;; store data everytime the balance changes
(defmethod (setf trader-balance) :after (value (reporting-trader reporting-trader))
  (setf (aref (balance-report reporting-trader) *current-date*) value))

;; we find out if a order was successful when it concludes
(defmethod conclude :around ((reporting-trader reporting-trader) &optional final)
  (let ((order (current-order reporting-trader))
        (result (call-next-method)))
    (when result
      (push order (order-list reporting-trader)))
    result))

(defmethod prepare-report ((reporting-trader reporting-trader))
  (with-accessors ((balance-report balance-report)) reporting-trader
    (let1 (history-length (length balance-report))
      ;; make sure the initial balance is present
      (assert (aref balance-report (- history-length 1)))
      (do ((i (- history-length 2) (- i 1))
           (j (- history-length 1) i))
          ((< i 0) balance-report)
        (unless (aref balance-report i)
          (setf (aref balance-report i) (aref balance-report j)))))))

(defclass debugging-trader ()
  ())

(defmethod long :before ((debugging-trader debugging-trader) part)
  (dbug "Started long trade on date ~A for part ~A" *current-date* part))

(defmethod short :before ((debugging-trader debugging-trader) part)
  (dbug "Started short trade on date ~A for part ~A" *current-date* part))

(defmethod conclude :before ((debugging-trader debugging-trader) &optional final)
   (dbug "Concluded trade ~A on date ~A " (current-order debugging-trader) *current-date*))

(defmethod past-data ((stock stock) &optional count)
  (stock-subseq stock (+ *current-date* 1) (if count (+ *current-date* count 1))))

(defmethod past-data ((stock-subseq stock-subseq) &optional count)
  (let ((start (max (+ *current-date* 1) (start stock-subseq))))
    (stock-subseq (parent stock-subseq) start
                  (if count
                      (min (+ start count) (end stock-subseq))
                      (end stock-subseq)))))

(defgeneric trade (trader))

(defun simulate-trader (trader initial-balance &key (start-after 0))
  (let* ((stock (trader-stock trader))
         (history-length (length (stock-date stock))))
    (let ((*current-date* (- history-length 1)))
      (setf (trader-balance trader) initial-balance)
      (decf *current-date* start-after)
      ;; main trading loop: call the trade every day
      (do ()
          ((< *current-date* 0))
        (trade trader)
        (decf *current-date*))
      ;; reset date to 0
      (setf *current-date* 0)
      ;; if trader still has an order, conclude it
      (when (busy-p trader)
        (conclude trader t))
      (prepare-report trader)
      (trader-balance trader))))

(defun gnuplot-date-tranform (dashed-date)
  (format nil "~A/~A/~A"
          (subseq dashed-date 5 7)
          (subseq dashed-date 8 10)
          (subseq dashed-date 0 4)))

(defgeneric plot-object (object))

(defmethod plot-object ((stock stock))
  ;; (eazy-gnuplot:gp :set :xdata :time)
  ;; (eazy-gnuplot:gp :set :timefmt "%m/%d/%y")
  (eazy-gnuplot:plot (lambda ()
                       (map nil (lambda (d o l h c)
                                  ;; date open low high close
                                  (format t "~&~A ~A ~A ~A ~A" (gnuplot-date-tranform d) o l h c))
                            (reverse (stock-date stock))
                            (reverse (stock-open stock))
                            (reverse (stock-low stock))
                            (reverse (stock-high stock))
                            (reverse (stock-close stock))))
                     :using '(0 2 3 4 5)
                     :with 'financebars))

(defun plot* (output &rest objects)
  (eazy-gnuplot:with-plots (*standard-output* :debug nil)
    (eazy-gnuplot:gp-setup :terminal '(pngcairo) :output output :bars 2)
    (dolist (o objects)
      (plot-object o)))
  output)

(plot* "plot-1.png" *current-stock*)
(plot* "plot-2.png" (stock-subseq *current-stock* 2000))

(defmethod plot-object ((trader reporting-trader))
  (eazy-gnuplot:plot (lambda ()
                       (map nil (lambda (d b) 
                                  (format t "~%~A ~A" (gnuplot-date-tranform d) b))
                            (reverse (stock-date (trader-stock trader)))
                            (reverse (balance-report trader))))
                     :using '(0 2)
                     :with 'lines))

(defun average (vector)
  (/ (reduce #'+ vector) (length vector)))

(defun bins-average (vector &optional (bin-count 2))
  "Split vector into `bin-count' equally large sequences and a list of averages."
  (assert (<= 2 bin-count))
  (let* ((bin-length (ceiling (length vector) bin-count))
         (bins (iter (for i from 0 below bin-count)
                     (collect (subseq vector (* i bin-length) (* (+ i 1) bin-length))))))
     (mapcar #'average bins)))

(defpar *optimistic-investment-factor* 2/3)
(defpar *pessimistic-investment-factor* 1/2)
(defpar *significance-level* 2)

(defun trend (list-2)
  "For a two element list, check whether there is a significant
  downward (negative) or upward (positive) trend. Note that the chronological order
  goes backward (start of `list-2' is more recent)."
  (let ((diff (- (first list-2) (second list-2))))
    (if (<= *significance-level* (abs diff))
        diff
        0)))

(defclass/f randomised-average-cmp-trader (reporting-trader debugging-trader)
  (observation-frame-length))

(defmethod trade ((trader randomised-average-cmp-trader))
  (if (current-order trader)
      ;; if we have a trade going, check out the close data
      (let ((trend (trend (bins-average
                           (stock-close (past-data
                                         (trader-stock trader)
                                         (observation-frame-length trader)))))))
        (when (or
               ;; upwards trend is bad for shorting
               (and (eql (order-type (current-order trader)) :short)
                    (< 0 trend))
               ;; downwards trend is bad for longing
               (and (eql (order-type (current-order trader)) :long)
                    (> 0 trend)))
          (conclude trader)))
      ;; if we have no trade going, check the open data
      (let ((trend (trend (bins-average
                           (stock-open (past-data
                                        (trader-stock trader)
                                        (observation-frame-length trader)))))))
        (cond ((< 0 trend)
               (long trader *pessimistic-investment-factor*))
              ((< (* 2 *significance-level*) trend)
               (long trader *optimistic-investment-factor*))
              ((> 0 trend)
               (short trader *pessimistic-investment-factor*))
              ((> (* -2 *significance-level*) trend)
               (short trader *optimistic-investment-factor*))))))

(setf *optimistic-investment-factor* 2/7
      *pessimistic-investment-factor* 6/7
      *significance-level* 1.6)

(defpar *current-trader*
        (make-instance 'randomised-average-cmp-trader
                       :observation-frame-length 84
                       :trader-stock *current-stock*))

(simulate-trader *current-trader* 100000
                 :start-after (observation-frame-length *current-trader*))

(plot* "plot-3.png" *current-trader*)
(format nil "~F" (trader-balance *current-trader*))
;; (balance-report *current-trader*)

(mapcar #'order-duration (reverse (order-list *current-trader*)))

(defun trader-order-table (trader)
  (let ((date-array (stock-date (trader-stock trader))))
    (cons (list "Type" "Volume" "Start" "End" "Duration")
          (mapcar (lambda (o)
                    (list (order-type o)
                          (order-volume o)
                          (aref date-array (order-start o))
                          (aref date-array (order-end o))
                          (order-duration o)))
                  (reverse (order-list trader))))))

(trader-order-table *current-trader*)
