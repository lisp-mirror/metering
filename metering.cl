;;; -*- Mode: LISP; Package: monitor; Syntax: Common-lisp; Base: 10.;  -*- 
;;; Tue Jan 25 18:32:28 1994 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; metering.cl -- 56711 bytes

;;; ****************************************************************
;;; Metering System ************************************************
;;; ****************************************************************
;;;
;;; The Metering System is a portable Common Lisp code profiling tool.
;;; It gathers timing and consing statistics for specified functions 
;;; while a program is running.
;;;
;;; The Metering System is a combination of 
;;;   o  the Monitor package written by Chris McConnell
;;;   o  the Profile package written by Skef Wholey and Rob MacLachlan
;;; The two systems were merged and extended by Mark Kantrowitz.
;;; Metering System was refreshed and adaptated to the 2016 realities
;;; by Daniel Kochmański.
;;; 
;;; This code is in the public domain and is distributed without warranty
;;; of any kind. 
;;;
;;; Bug reports, comments, and suggestions should be sent to
;;; daniel@turtleware.eu.

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; 26-JUN-90  mk       Merged functionality of Monitor and Profile packages.
;;; 26-JUN-90  mk       Now handles both inclusive and exclusive statistics
;;;                     with respect to nested calls. (Allows it to subtract
;;;                     total monitoring overhead for each function, not just
;;;                     the time spent monitoring the function itself.)
;;; 26-JUN-90  mk       The table is now saved so that one may manipulate
;;;                     the data (sorting it, etc.) even after the original
;;;                     source of the data has been cleared.
;;; 25-SEP-90  mk       Added get-cons functions for Lucid 3.0, MACL 1.3.2
;;;                     required-arguments functions for Lucid 3.0,
;;;                     Franz Allegro CL, and MACL 1.3.2.
;;; 25-JAN-91  mk       Now uses fdefinition if available.
;;; 25-JAN-91  mk       Replaced (and :allegro (not :coral)) with :excl.
;;;                     Much better solution for the fact that both call 
;;;                     themselves :allegro.
;;;  5-JUL-91 mk        Fixed warning to occur only when file is loaded 
;;;                     uncompiled.
;;;  5-JUL-91 mk        When many unmonitored functions, print out number 
;;;                     instead of whole list.
;;; 24-MAR-92 mk        Updated for CLtL2 compatibility. space measuring 
;;;                     doesn't work in MCL, but fixed so that timing 
;;;                     statistics do.
;;; 26-MAR-92 mk        Updated for Lispworks. Replaced :ccl with 
;;;                     (and :ccl (not :lispworks)).
;;; 27-MAR-92 mk        Added get-cons for Allegro-V4.0.
;;; 01-JAN-93 mk  v2.0  Support for MCL 2.0, CMU CL 16d, Allegro V3.1/4.0/4.1, 
;;;                     Lucid 4.0, ibcl
;;; 25-JAN-94 mk  v2.1  Patches for CLISP from Bruno Haible.
;;; 13-MAR-16 dk  v3.0  General clean-up - removal of the obsolete
;;;                     implementations support and constructs, testing on
;;;                     the active implementations etc.
;;;

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;;    - Speed up monitoring code. Replace use of hash tables with an embedded
;;;      offset in an array so that it will be faster than using gethash. 
;;;      (i.e., svref/closure reference is usually faster than gethash).
;;;    - Beware of (get-internal-run-time) overflowing. Yikes!
;;;    - Check robustness with respect to profiled functions.
;;;    - Check logic of computing inclusive and exclusive time and consing.
;;;      Especially wrt incf/setf comment below. Should be incf, so we
;;;      sum recursive calls.
;;;    - Add option to record caller statistics -- this would list who
;;;      called which functions and how often.
;;;    - Switches to turn timing/CONSING statistics collection on/off.

;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    METERING has been tested (successfully) in the following lisps:
;;;       CLISP (January 1994)
;;;
;;;    METERING needs to be tested in the following lisps:
;;;       CLISP (2.49+)

;;; ****************************************************************
;;; Documentation **************************************************
;;; ****************************************************************
;;;
;;; This system runs in any valid Common Lisp. Four small 
;;; implementation-dependent changes can be made to improve performance
;;; and prettiness. In the section labelled "Implementation Dependent
;;; Changes" below, you should tailor the functions REQUIRED-ARGUMENTS,
;;; GET-CONS, GET-TIME, and TIME-UNITS-PER-SECOND to your implementation
;;; for the best results. If GET-CONS is not specified for your 
;;; implementation, no consing information will be reported. The other
;;; functions will default to working forms, albeit inefficient, in
;;; non-CMU implementations. If you tailor these functions for a particular
;;; version of Common Lisp, we'd appreciate receiving the code.
;;;

;;; ****************************************************************
;;; Usage Notes ****************************************************
;;; ****************************************************************
;;; 
;;; SUGGESTED USAGE:
;;; 
;;; Start by monitoring big pieces of the program, then carefully choose
;;; which functions close to, but not in, the inner loop are to be 
;;; monitored next. Don't monitor functions that are called by other
;;; monitored functions: you will only confuse yourself.
;;;
;;; If the per-call time reported is less than 1/10th of a second, then
;;; consider the clock resolution and profiling overhead before you believe
;;; the time. It may be that you will need to run your program many times
;;; in order to average out to a higher resolution.
;;;
;;; The easiest way to use this package is to load it and execute either
;;;     (mon:with-monitoring (names*) ()
;;;         your-forms*)
;;; or                      
;;;     (mon:monitor-form your-form)
;;; The former allows you to specify which functions will be monitored; the
;;; latter monitors all functions in the current package. Both automatically
;;; produce a table of statistics. Other variants can be constructed from
;;; the monitoring primitives, which are described below, along with a
;;; fuller description of these two macros.
;;; 
;;; For best results, compile this file before using.
;;;
;;; 
;;; CLOCK RESOLUTION:
;;; 
;;; Unless you are very lucky, the length of your machine's clock "tick" is
;;; probably much longer than the time it takes a simple function to run.
;;; For example, on the IBM RT, the clock resolution is 1/50th of a second.
;;; This means that if a function is only called a few times, then only the
;;; first couple of decimal places are really meaningful.
;;;
;;; 
;;; MONITORING OVERHEAD:
;;;
;;; The added monitoring code takes time to run every time that the monitored
;;; function is called, which can disrupt the attempt to collect timing 
;;; information. In order to avoid serious inflation of the times for functions
;;; that take little time to run, an estimate of the overhead due to monitoring
;;; is subtracted from the times reported for each function. 
;;; 
;;; Although this correction works fairly well, it is not totally accurate,
;;; resulting in times that become increasingly meaningless for functions
;;; with short runtimes. For example, subtracting the estimated overhead
;;; may result in negative times for some functions. This is only a concern
;;; when the estimated profiling overhead is many times larger than 
;;; reported total CPU time.
;;;
;;; If you monitor functions that are called by monitored functions, in
;;; :inclusive mode the monitoring overhead for the inner function is
;;; subtracted from the CPU time for the outer function. [We do this by
;;; counting for each function not only the number of calls to *this*
;;; function, but also the number of monitored calls while it was running.]
;;; In :exclusive mode this is not necessary, since we subtract the
;;; monitoring time of inner functions, overhead & all.
;;;
;;; Otherwise, the estimated monitoring overhead is not represented in the
;;; reported total CPU time. The sum of total CPU time and the estimated
;;; monitoring overhead should be close to the total CPU time for the
;;; entire monitoring run (as determined by TIME).
;;;
;;; A timing overhead factor is computed at load time. This will be incorrect
;;; if the monitoring code is run in a different environment than this file
;;; was loaded in. For example, saving a core image on a high performance
;;; machine and running it on a low performance one will result in the use
;;; of an erroneously small overhead factor.
;;;
;;;
;;; If your times vary widely, possible causes are:
;;;    - Garbage collection.  Try turning it off, then running your code.
;;;      Be warned that monitoring code will probably cons when it does
;;;      (get-internal-run-time).
;;;    - Swapping.  If you have enough memory, execute your form once
;;;      before monitoring so that it will be swapped into memory. Otherwise,
;;;      get a bigger machine! 
;;;    - Resolution of internal-time-units-per-second.  If this value is
;;;      too low, then the timings become wild. You can try executing more
;;;      of whatever your test is, but that will only work if some of your
;;;      paths do not match the timer resolution. 
;;;      internal-time-units-per-second is so coarse -- on a Symbolics it is
;;;      977, in MACL it is 60.
;;;
;;;

;;; ****************************************************************
;;; Interface ******************************************************
;;; ****************************************************************
;;;
;;; WITH-MONITORING (&rest functions)                         [Macro]
;;;                 (&optional (nested :exclusive) 
;;;                            (threshold 0.01)
;;;                            (key :percent-time))
;;;                 &body body
;;; The named functions will be set up for monitoring, the body forms executed,
;;; a table of results printed, and the functions unmonitored. The nested,
;;; threshold, and key arguments are passed to report-monitoring below.
;;;
;;; MONITOR-FORM form                                         [Macro]
;;;               &optional (nested :exclusive)
;;;                         (threshold 0.01)
;;;                         (key :percent-time)
;;; All functions in the current package are set up for monitoring while
;;; the form is executed, and automatically unmonitored after a table of
;;; results has been printed. The nested, threshold, and key arguments 
;;; are passed to report-monitoring below.
;;;
;;; *MONITORED-FUNCTIONS*                                     [Variable]
;;; This holds a list of all functions that are currently being monitored.
;;;
;;; MONITOR &rest names                                       [Macro]
;;; The named functions will be set up for monitoring by augmenting
;;; their function definitions with code that gathers statistical information
;;; about code performance. As with the TRACE macro, the function names are
;;; not evaluated. Calls the function MON::MONITORING-ENCAPSULATE on each
;;; function name. If no names are specified, returns a list of all 
;;; monitored functions.
;;; 
;;; If name is not a symbol, it is evaled to return the appropriate
;;; closure. This allows you to monitor closures stored anywhere like
;;; in a variable, array or structure. Most other monitoring packages 
;;; can't handle this. 
;;;
;;; MONITOR-ALL &optional (package *package*)                 [Function]
;;; Monitors all functions in the specified package, which defaults to
;;; the current package.
;;;
;;; UNMONITOR &rest names                                     [Macro]
;;; Removes monitoring code from the named functions. If no names are
;;; specified, all currently monitored functions are unmonitored.
;;;
;;; RESET-MONITORING-INFO name                                [Function]
;;; Resets the monitoring statistics for the specified function.
;;; 
;;; RESET-ALL-MONITORING                                      [Function]
;;; Resets the monitoring statistics for all monitored functions.
;;;
;;; MONITORED name                                            [Function]
;;; Predicate to test whether a function is monitored.
;;; 
;;; REPORT-MONITORING &optional names                         [Function]
;;;                             (nested :exclusive) 
;;;                             (threshold 0.01)
;;;                             (key :percent-time)
;;; Creates a table of monitoring information for the specified list
;;; of names, and displays the table using display-monitoring-results.
;;; If names is :all or nil, uses all currently monitored functions.
;;; Takes the following arguments:
;;;    - NESTED specifies whether nested calls of monitored functions
;;;      are included in the times for monitored functions.
;;;      o  If :inclusive, the per-function information is for the entire
;;;         duration of the monitored function, including any calls to
;;;         other monitored functions. If functions A and B are monitored,
;;;         and A calls B, then the accumulated time and consing for A will
;;;         include the time and consing of B.  Note: if a function calls
;;;         itself recursively, the time spent in the inner call(s) may
;;;         be counted several times.
;;;      o  If :exclusive, the information excludes time attributed to
;;;         calls to other monitored functions. This is the default.
;;;    - THRESHOLD specifies that only functions which have been executed
;;;      more than threshold percent of the time will be reported. Defaults
;;;      to 1%. If a threshold of 0 is specified, all functions are listed,
;;;      even those with 0 or negative running times (see note on overhead).
;;;    - KEY specifies that the table be sorted by one of the following
;;;      sort keys:
;;;         :function       alphabetically by function name
;;;         :percent-time   by percent of total execution time
;;;         :percent-cons   by percent of total consing
;;;         :calls          by number of times the function was called
;;;         :time-per-call  by average execution time per function
;;;         :cons-per-call  by average consing per function
;;;         :time           same as :percent-time
;;;         :cons           same as :percent-cons
;;;
;;; DISPLAY-MONITORING-RESULTS &optional (threshold 0.01)     [Function]
;;;                                      (key :percent-time)
;;; Prints a table showing for each named function:
;;;    - the total CPU time used in that function for all calls
;;;    - the total number of bytes consed in that function for all calls
;;;    - the total number of calls
;;;    - the average amount of CPU time per call
;;;    - the average amount of consing per call
;;;    - the percent of total execution time spent executing that function
;;;    - the percent of total consing spent consing in that function
;;; Summary totals of the CPU time, consing, and calls columns are printed.
;;; An estimate of the monitoring overhead is also printed. May be run
;;; even after unmonitoring all the functions, to play with the data.
;;;
;;; SAMPLE TABLE:
#|
                                               Cons
                 %     %                       Per      Total   Total
Function         Time  Cons  Calls  Sec/Call   Call     Time    Cons
----------------------------------------------------------------------
FIND-ROLE:       0.58  0.00    136  0.003521      0  0.478863       0
GROUP-ROLE:      0.35  0.00    365  0.000802      0  0.292760       0
GROUP-PROJECTOR: 0.05  0.00    102  0.000408      0  0.041648       0
FEATURE-P:       0.02  0.00    570  0.000028      0  0.015680       0
----------------------------------------------------------------------
TOTAL:                        1173                   0.828950       0
Estimated total monitoring overhead: 0.88 seconds
|#

;;; ****************************************************************
;;; METERING *******************************************************
;;; ****************************************************************

;;; ********************************
;;; Fix up the *features* list *****
;;; ********************************

(eval-when (compile load eval) #| nothing to fix |# )

;;; ********************************
;;; Packages ***********************
;;; ********************************

(defpackage "MONITOR" (:nicknames "MON") (:use "COMMON-LISP"))
(in-package "MONITOR")

(provide "monitor")

(export '(*monitored-functions*
	  monitor monitor-all unmonitor monitor-form
	  with-monitoring
	  reset-monitoring-info reset-all-monitoring
	  monitored
	  report-monitoring
	  display-monitoring-results
	  monitoring-encapsulate monitoring-unencapsulate))


;;; Warn user if they're loading the source instead of compiling it first.
(eval-when (eval)
   (warn "This file should be compiled before loading for best results."))

;;; ********************************
;;; Version ************************
;;; ********************************

(defparameter *metering-version* "v3.0 13-MAR-16"
  "Current version number/date for Metering.")


;;; ****************************************************************
;;; Implementation Dependent Definitions ***************************
;;; ****************************************************************

;;; ********************************
;;; Type Definitions ***************
;;; ********************************

(eval-when (compile load eval)
  (deftype time-type () 'unsigned-byte)
  (deftype consing-type () 'unsigned-byte))

;;; ********************************
;;; Timing Functions ***************
;;; ********************************
;;; The get-time function is called to find the total number of ticks since
;;; the beginning of time. time-units-per-second allows us to convert units
;;; to seconds.

(progn
  #-(or :cmu :clisp :clozure :allegro)
  (eval-when (compile eval)
    (warn
     "You may want to supply implementation-specific get-time functions."))

  (defconstant time-units-per-second internal-time-units-per-second)
  
  (defmacro get-time ()
    `(the time-type (get-internal-run-time)))
)

;;; ********************************
;;; Consing Functions **************
;;; ********************************
;;; The get-cons macro is called to find the total number of bytes
;;; consed since the beginning of time.

#+:cmu
(defmacro get-cons ()
  "The get-cons macro is called to find the total number of bytes
   consed since the beginning of time."
  '(the consing-type (ext:get-bytes-consed)))

#+:clisp
(defun get-cons ()
  (multiple-value-bind (real1 real2 run1 run2 gc1 gc2 space1 space2 gccount)
      (sys::%%time)
    (declare (ignore real1 real2 run1 run2 gc1 gc2 gccount))
    (dpb space1 (byte 24 24) space2)))

;;; Clozure Common Lisp
;;; Note that this includes bytes that were allocated during GC.  We
;;; could subtract this out by advising GC, but I'd rather users ran
;;; without GC. If they can't run without GC, then the bytes consed
;;; during GC are a cost of running their program. Metering the code a
;;; few times will avoid the consing values being too lopsided. If a
;;; user really really wants to subtract out the consing during GC,
;;; replace the following two lines with the commented out code.
#+:clozure
(defmacro get-cons () `(the consing-type (ccl::total-bytes-allocated)))

#-(or :cmu :clisp :clozure)
(progn
  (eval-when (compile eval)
    (warn "No consing will be reported unless a get-cons function is ~
           defined."))

  (defmacro get-cons () '(the consing-type 0)))

;;; ********************************
;;; Required Arguments *************
;;; ********************************
;;;
;;; Required (Fixed) vs Optional Args
;;;
;;; To avoid unnecessary consing in the "encapsulation" code, we find out the
;;; number of required arguments, and use &rest to capture only non-required
;;; arguments.  The function Required-Arguments returns two values: the first
;;; is the number of required arguments, and the second is T iff there are any
;;; non-required arguments (e.g. &optional, &rest, &key).
#+cmu
(defun required-arguments (name)
  (let ((type (ext:info function type name)))
    (cond ((not (kernel:function-type-p type))
	   (warn "No argument count information available for:~%  ~S~@
		  Allow for &rest arg consing."
		 name)
	   (values 0 t))
	  (t
	   (values (length (kernel:function-type-required type))
		   (if (or (kernel:function-type-optional type)
			   (kernel:function-type-keyp type)
			   (kernel:function-type-rest type))
		       t nil))))))

;;; Allegro and Clozure Common Lisp
#+(or allegro clozure)
(defun required-arguments (name)
  (let* ((function (symbol-function name))
         (args #+:allegro(excl::arglist function)
	       #+:clozure(ccl:arglist function))
         (pos (position-if #'(lambda (x)
                               (and (symbolp x)
                                    (let ((name (symbol-name x)))
                                      (and (>= (length name) 1)
                                           (char= (schar name 0)
                                                  #\&)))))
                           args)))
    (if pos
        (values pos t)
      (values (length args) nil))))

#+:clisp
(defun required-arguments (name)
  (let ((function (symbol-function name)))
    (case (type-of function)
      (FUNCTION
       (if (compiled-function-p function)
	   (multiple-value-bind (req-anz opt-anz rest-p key-p
					 keyword-list allow-other-keys-p)
	       (sys::signature function)
	     (declare (ignore keyword-list allow-other-keys-p))
	     (values req-anz (or (plusp opt-anz) rest-p key-p)))
	   (let ((lambdalist (car (sys::%record-ref function 1))))
	     (values (or (position-if #'(lambda (x) 
					  (member x lambda-list-keywords))
				      lambdalist)
			 (length lambdalist))
		     (and (intersection lambdalist lambda-list-keywords) t)))))
      (COMPILED-FUNCTION
       (multiple-value-bind (name req-anz opt-anz rest-p
				  keywords allow-other-keys)
	   (sys::subr-info function)
	 (declare (ignore allow-other-keys))
	 (if name
	     (values req-anz (or (plusp opt-anz) rest-p keywords))
	     (values 0 t))))
      (T (values 0 t)))))

#-(or :cmu :clisp :clozure :allegro)
(progn
  (eval-when (compile eval)
    (warn
     "You may want to add an implementation-specific Required-Arguments function."))
  (eval-when (load eval)
    (defun required-arguments (name)
      (declare (ignore name))
      (values 0 t))))

#|
;;;Examples
(defun square (x) (* x x))
(defun square2 (x &optional y) (* x x y))
(defun test (x y &optional (z 3)) 3)
(defun test2 (x y &optional (z 3) &rest fred) 3)

(required-arguments 'square) => 1 nil
(required-arguments 'square2) => 1 t
(required-arguments 'test) => 2 t
(required-arguments 'test2) => 2 t
|#

;;; ********************************
;;; Fdefinition ********************
;;; ********************************
;;; fdefinition is a CLtL2 addition. 
#+(and :cmu (not (or new-compiler :new-compiler)))
(eval-when (compile eval)
  ;; Need to worry about extensions:encapsulate in CMU CL
  ;; Note: We should really be defining fdefinition as a function
  ;; in the "LISP" package and export it. But this will do for now,
  ;; especially since we only define it while compiling this code.
  ;; The use of (fboundp 'fdefinition) later in this file works
  ;; because (fboundp <macro>) returns t.
  ;; (export 'lisp::fdefinition "LISP")
  (defmacro fdefinition (x)
    `(lisp::careful-symbol-function ,x))
  (defsetf fdefinition lisp::set-symbol-function-carefully))


;;; ****************************************************************
;;; Main METERING Code *********************************************
;;; ****************************************************************

;;; ********************************
;;; Global Variables ***************
;;; ********************************
(defvar *MONITOR-TIME-OVERHEAD* nil
  "The amount of time an empty monitored function costs.")
(defvar *MONITOR-CONS-OVERHEAD* nil
  "The amount of cons an empty monitored function costs.")

(defvar *TOTAL-TIME* 0
  "Total amount of time monitored so far.")
(defvar *TOTAL-CONS* 0
  "Total amount of consing monitored so far.")
(defvar *TOTAL-CALLS* 0
  "Total number of calls monitored so far.")
(proclaim '(type time-type *total-time*))
(proclaim '(type consing-type *total-cons*))
(proclaim '(fixnum *total-calls*))

;;; ********************************
;;; Accessor Functions *************
;;; ********************************
;;; Perhaps the SYMBOLP should be FBOUNDP? I.e., what about variables
;;; containing closures.
(defmacro PLACE-FUNCTION (function-place)
  "Return the function found at FUNCTION-PLACE. Evals FUNCTION-PLACE
if it isn't a symbol, to allow monitoring of closures located in
variables/arrays/structures."
  ;; Note that (fboundp 'fdefinition) returns T even if fdefinition
  ;; is a macro, which is what we want.
  (if (fboundp 'fdefinition)
      `(if (fboundp ,function-place)
	   (fdefinition ,function-place)
	   (eval ,function-place))
      `(if (symbolp ,function-place)
	   (symbol-function ,function-place)
	   (eval ,function-place))))

(defsetf PLACE-FUNCTION (function-place) (function)
  "Set the function in FUNCTION-PLACE to FUNCTION."
  (if (fboundp 'fdefinition)
      ;; If we're conforming to CLtL2, use fdefinition here.
      `(if (fboundp ,function-place)
	   (setf (fdefinition ,function-place) ,function) 
	   (eval '(setf ,function-place ',function)))
      `(if (symbolp ,function-place)
	   (setf (symbol-function ,function-place) ,function) 
	   (eval '(setf ,function-place ',function)))))

#|
;;; before using fdefinition
(defun PLACE-FUNCTION (function-place)
  "Return the function found at FUNCTION-PLACE. Evals FUNCTION-PLACE
if it isn't a symbol, to allow monitoring of closures located in
variables/arrays/structures."
  (if (symbolp function-place)
      (symbol-function function-place)
      (eval function-place)))

(defsetf PLACE-FUNCTION (function-place) (function)
  "Set the function in FUNCTION-PLACE to FUNCTION."
  `(if (symbolp ,function-place)
       (setf (symbol-function ,function-place) ,function)
       (eval '(setf ,function-place ',function))))
|#

(defun PLACE-FBOUNDP (function-place)
  "Test to see if FUNCTION-PLACE is a function."
  ;; probably should be 
  #|(or (and (symbolp function-place)(fboundp function-place))
      (functionp (place-function function-place)))|#
  (if (symbolp function-place)
      (fboundp function-place)
      (functionp (place-function function-place))))

(defun PLACE-MACROP (function-place)
  "Test to see if FUNCTION-PLACE is a macro."
  (when (symbolp function-place)
    (macro-function function-place)))

;;; ********************************
;;; Measurement Tables *************
;;; ********************************
(defvar *monitored-functions* nil
  "List of monitored symbols.")

;;; We associate a METERING-FUNCTIONS structure with each monitored function
;;; name or other closure. This holds the functions that we call to manipulate
;;; the closure which implements the encapsulation.
;;;
(defstruct metering-functions
  (name nil)
  (old-definition (error "Missing required keyword argument :old-definition")
		  :type function)
  (new-definition (error "Missing required keyword argument :new-definition")
		  :type function)
  (read-metering  (error "Missing required keyword argument :read-metering")
		  :type function)
  (reset-metering (error "Missing required keyword argument :reset-metering")
		  :type function))

;;; In general using hash tables in time-critical programs is a bad idea,
;;; because when one has to grow the table and rehash everything, the
;;; timing becomes grossly inaccurate. In this case it is not an issue
;;; because all inserting of entries in the hash table occurs before the
;;; timing commences. The only circumstance in which this could be a 
;;; problem is if the lisp rehashes on the next reference to the table,
;;; instead of when the entry which forces a rehash was inserted. 
;;;
;;; Note that a similar kind of problem can occur with GC, which is why
;;; one should turn off GC when monitoring code. 
;;;
(defvar *monitor* (make-hash-table :test #'equal)
  "Hash table in which METERING-FUNCTIONS structures are stored.")
(defun get-monitor-info (name)
  (gethash name *monitor*))
(defsetf get-monitor-info (name) (info)
  `(setf (gethash ,name *monitor*) ,info))

(defun MONITORED (function-place)
  "Test to see if a FUNCTION-PLACE is monitored."
  (and (place-fboundp function-place)	; this line necessary?
       (get-monitor-info function-place)))

(defun reset-monitoring-info (name)
  "Reset the monitoring info for the specified function."
  (let ((finfo (get-monitor-info name)))
    (when finfo
      (funcall (metering-functions-reset-metering finfo)))))
(defun reset-all-monitoring () 
  "Reset monitoring info for all functions."
  (setq *total-time* 0
	*total-cons* 0
	*total-calls* 0)
  (dolist (symbol *monitored-functions*)
    (when (monitored symbol)
      (reset-monitoring-info symbol))))

(defun monitor-info-values (name &optional (nested :exclusive) warn)
  "Returns monitoring information values for the named function,
adjusted for overhead."
  (let ((finfo (get-monitor-info name)))
    (if finfo
	(multiple-value-bind (inclusive-time inclusive-cons
					     exclusive-time exclusive-cons
					     calls nested-calls)
	    (funcall (metering-functions-read-metering finfo))
	  (unless (or (null warn)
		      (eq (place-function name)
			  (metering-functions-new-definition finfo)))
	    (warn "Funtion ~S has been redefined, so times may be inaccurate.~@
                   MONITOR it again to record calls to the new definition."
		  name))
	  (case nested
	    (:exclusive (values calls
				nested-calls
				(- exclusive-time 
				   (* calls *monitor-time-overhead*))
				(- exclusive-cons 
				   (* calls *monitor-cons-overhead*))))
	    ;; In :inclusive mode, subtract overhead for all the
	    ;; called functions as well. Nested-calls includes the
	    ;; calls of the function as well. [Necessary 'cause of
	    ;; functions which call themselves recursively.]
	    (:inclusive (values calls
				nested-calls
				(- inclusive-time 
				   (* nested-calls ;(+ calls)
				      *monitor-time-overhead*))
				(- inclusive-cons 
				   (* nested-calls ;(+ calls)
				      *monitor-cons-overhead*))))))
	(values 0 0 0 0))))

;;; ********************************
;;; Encapsulate ********************
;;; ********************************
(eval-when (compile load eval)
;; Returns a lambda expression for a function that, when called with the
;; function name, will set up that function for metering.
;;
;; A function is monitored by replacing its definition with a closure
;; created by the following function. The closure records the monitoring
;; data, and updates the data with each call of the function.
;;
;; Other closures are used to read and reset the data.
(defun make-monitoring-encapsulation (min-args optionals-p)
  (let (required-args)
    (dotimes (i min-args) (push (gensym) required-args))
    `(lambda (name)
       (let ((inclusive-time 0)
	     (inclusive-cons 0)
	     (exclusive-time 0)
	     (exclusive-cons 0)
	     (calls 0)
	     (nested-calls 0)
	     (old-definition (place-function name)))
	 (declare (type time-type inclusive-time)
		  (type time-type exclusive-time)
		  (type consing-type inclusive-cons)
		  (type consing-type exclusive-cons)
		  (fixnum calls)
		  (fixnum nested-calls))
	 (pushnew name *monitored-functions*)

	 (setf (place-function name)
	       #'(lambda (,@required-args
			  ,@(when optionals-p `(&rest optional-args)))
		   (let ((prev-total-time *total-time*)
			 (prev-total-cons *total-cons*)
			 (prev-total-calls *total-calls*)
;			 (old-time inclusive-time)
;			 (old-cons inclusive-cons)
;			 (old-nested-calls nested-calls)
			 (start-time (get-time))
			 (start-cons (get-cons)))
		     (declare (type time-type prev-total-time)
			      (type time-type start-time)
			      (type consing-type prev-total-cons)
			      (type consing-type start-cons)
			      (fixnum prev-total-calls))
		     (multiple-value-prog1
			 ,(if optionals-p
			      `(apply old-definition 
				      ,@required-args optional-args)
			      `(funcall old-definition ,@required-args))
		       (let ((delta-time (- (get-time) start-time))
			     (delta-cons (- (get-cons) start-cons)))
			 ;; Calls
			 (incf calls)
			 (incf *total-calls*)
			    ;;; nested-calls includes this call
			 (incf nested-calls (the fixnum 
						 (- *total-calls*
						    prev-total-calls)))
;			 (setf nested-calls (+ old-nested-calls
;					       (- *total-calls*
;						  prev-total-calls)))
			 ;; Time
			    ;;; Problem with inclusive time is that it
			    ;;; currently doesn't add values from recursive
			    ;;; calls to the same function. Change the
			    ;;; setf to an incf to fix this?
			 (incf inclusive-time (the time-type delta-time))
;			 (setf inclusive-time (+ delta-time old-time))
			 (incf exclusive-time (the time-type
						   (+ delta-time
						      (- prev-total-time 
							 *total-time*))))
			 (setf *total-time* (the time-type
						 (+ delta-time
						    prev-total-time)))
			 ;; Consing
			 (incf inclusive-cons (the consing-type delta-cons))
;			 (setf inclusive-cons (+ delta-cons old-cons))
			 (incf exclusive-cons (the consing-type 
						   (+ delta-cons
						      (- prev-total-cons 
							 *total-cons*))))
			 (setf *total-cons* 
			       (the consing-type 
				    (+ delta-cons prev-total-cons))))))))
	 (setf (get-monitor-info name)
	       (make-metering-functions
		:name name
		:old-definition old-definition
		:new-definition (place-function name)
		:read-metering #'(lambda ()
				   (values inclusive-time
					   inclusive-cons
					   exclusive-time
					   exclusive-cons
					   calls
					   nested-calls))
		:reset-metering #'(lambda ()
				    (setq inclusive-time 0
					  inclusive-cons 0
					  exclusive-time 0 
					  exclusive-cons 0
					  calls 0
					  nested-calls 0)
				    t)))))))
);; End of EVAL-WHEN

;;; For efficiency reasons, we precompute the encapsulation functions
;;; for a variety of combinations of argument structures 
;;; (min-args . optional-p). These are stored in the following hash table
;;; along with any new ones we encounter. Since we're now precomputing
;;; closure functions for common argument signatures, this eliminates
;;; the former need to call COMPILE for each monitored function.  
(eval-when (compile eval)
   (defconstant precomputed-encapsulations 8))

(defvar *existing-encapsulations* (make-hash-table :test #'equal))
(defun find-encapsulation (min-args optionals-p)
  (or (gethash (cons min-args optionals-p) *existing-encapsulations*)
      (setf (gethash (cons min-args optionals-p) *existing-encapsulations*)
	    (compile nil
		     (make-monitoring-encapsulation min-args optionals-p)))))

(macrolet ((frob ()
	     (let ((res ()))
	       (dotimes (i precomputed-encapsulations)
		 (push `(setf (gethash '(,i . nil) *existing-encapsulations*)
			      #',(make-monitoring-encapsulation i nil))
		       res)
		 (push `(setf (gethash '(,i . t) *existing-encapsulations*)
			      #',(make-monitoring-encapsulation i t))
		       res))
	       `(progn ,@res))))
  (frob))

(defun monitoring-encapsulate (name &optional warn)
  "Monitor the function Name. If already monitored, unmonitor first."
  ;; Saves the current definition of name and inserts a new function which
  ;; returns the result of evaluating body. 
  (cond ((not (place-fboundp name))	; not a function
	 (when warn
	   (warn "Ignoring undefined function ~S." name)))
	((place-macrop name)		; a macro
	 (when warn
	   (warn "Ignoring macro ~S." name)))
	(t				; tis a function
	 (when (get-monitor-info name) ; monitored
	   (when warn
	     (warn "~S already monitored, so unmonitoring it first." name))
	   (monitoring-unencapsulate name))
	 (multiple-value-bind (min-args optionals-p)
	     (required-arguments name)
	   (funcall (find-encapsulation min-args optionals-p) name)))))

(defun monitoring-unencapsulate (name &optional warn)
  "Removes monitoring encapsulation code from around Name."
  (let ((finfo (get-monitor-info name)))
    (when finfo				; monitored
      (remprop name 'metering-functions)
      (setq *monitored-functions* 
	    (remove name *monitored-functions* :test #'equal))
      (if (eq (place-function name)
	      (metering-functions-new-definition finfo))
	  (setf (place-function name)
		(metering-functions-old-definition finfo))
	  (when warn
	    (warn "Preserving current definition of redefined function ~S."
		  name))))))
    
;;; ********************************
;;; Main Monitoring Functions ******
;;; ********************************
(defmacro MONITOR (&rest names)
  "Monitor the named functions. As in TRACE, the names are not evaluated.
   If a function is already monitored, then unmonitor and remonitor (useful
   to notice function redefinition). If a name is undefined, give a warning
   and ignore it. See also unmonitor, report-monitoring, 
   display-monitoring-results and reset-time."
  `(progn
     ,@(mapcar #'(lambda (name) `(monitoring-encapsulate ',name)) names)
     *monitored-functions*))

(defmacro UNMONITOR (&rest names)
  "Remove the monitoring on the named functions. 
   Names defaults to the list of all currently monitored functions."
  `(dolist (name ,(if names `',names '*monitored-functions*) (values))
     (monitoring-unencapsulate name)))		 

(defun MONITOR-ALL (&optional (package *package*))
  "Monitor all functions in the specified package."
  (let ((package (if (symbolp package)
		     (find-package package)
		     package)))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
	(monitoring-encapsulate symbol)))))

(defmacro MONITOR-FORM (form 
			&optional (nested :exclusive) (threshold 0.01)
			(key :percent-time))
  "Monitor the execution of all functions in the current package
during the execution of FORM.  All functions that are executed above
THRESHOLD % will be reported."
  `(unwind-protect
       (progn
	 (monitor-all)
	 (reset-all-monitoring)
	 (prog1
	     (time ,form)
	   (report-monitoring :all ,nested ,threshold ,key :ignore-no-calls)))
     (unmonitor)))

(defmacro WITH-MONITORING ((&rest functions)
			   (&optional (nested :exclusive) 
				      (threshold 0.01)
				      (key :percent-time))
			   &body body)
  "Monitor the specified functions during the execution of the body."
  `(unwind-protect
       (progn
	 (dolist (fun ',functions)
	   (monitoring-encapsulate fun))
	 (reset-all-monitoring)
	 ,@body
	 (report-monitoring :all ,nested ,threshold ,key))
     (unmonitor)))

;;; ********************************
;;; Overhead Calculations **********
;;; ********************************
(defconstant overhead-iterations 5000
  "Number of iterations over which the timing overhead is averaged.")

;;; Perhaps this should return something to frustrate clever compilers.
(defun STUB-FUNCTION (x)
  (declare (ignore x))
  nil)
(proclaim '(notinline stub-function))

(defun SET-MONITOR-OVERHEAD ()
  "Determines the average overhead of monitoring by monitoring the execution
of an empty function many times." 
  (setq *monitor-time-overhead* 0
	*monitor-cons-overhead* 0)
  (stub-function nil)
  (monitor stub-function)
  (reset-all-monitoring)
  (let ((overhead-function (symbol-function 'stub-function)))
    (dotimes (x overhead-iterations)
      (funcall overhead-function overhead-function)))
;  (dotimes (x overhead-iterations)
;    (stub-function nil))
  (let ((fiter (float overhead-iterations)))
    (multiple-value-bind (calls nested-calls time cons)
	(monitor-info-values 'stub-function)
      (declare (ignore calls nested-calls))
      (setq *monitor-time-overhead* (/ time fiter)
	    *monitor-cons-overhead* (/ cons fiter))))
  (unmonitor stub-function))
(set-monitor-overhead)

;;; ********************************
;;; Report Data ********************
;;; ********************************
(defvar *monitor-results* nil
  "A table of monitoring statistics is stored here.")
(defvar *no-calls* nil
  "A list of monitored functions which weren't called.")
(defvar *estimated-total-overhead* 0)
(proclaim '(type time-type *estimated-total-overhead*))

(defstruct (monitoring-info 
	    (:conc-name m-info-)
	    (:constructor make-monitoring-info
			  (name calls time cons
				percent-time percent-cons
				time-per-call cons-per-call)))
  name
  calls
  time
  cons
  percent-time
  percent-cons
  time-per-call
  cons-per-call)

(defun REPORT-MONITORING (&optional names 
				    (nested :exclusive) 
				    (threshold 0.01)
				    (key :percent-time)
				    ignore-no-calls)
  "Report the current monitoring state.
The percentage of the total time spent executing unmonitored code
in each function (:exclusive mode), or total time (:inclusive mode)
will be printed together with the number of calls and
the unmonitored time per call.  Functions that have been executed
below THRESHOLD % of the time will not be reported."
  (when (or (null names) (eq names :all)) (setq names *monitored-functions*))

  (let ((total-time 0)
	(total-cons 0)
	(total-calls 0))
    ;; Compute overall time and consing.
    (dolist (name names)
      (multiple-value-bind (calls nested-calls time cons)
	  (monitor-info-values name nested :warn)
	(declare (ignore nested-calls))
	(incf total-calls calls)
	(incf total-time time)
	(incf total-cons cons)))
    ;; Total overhead.
    (setq *estimated-total-overhead* 	    
	  (/ (* *monitor-time-overhead* total-calls)
	     time-units-per-second))
    ;; Assemble data for only the specified names (all monitored functions)
    (if (zerop total-time)
	(format *trace-output* "Not enough execution time to monitor.")
	(progn
	  (setq *monitor-results* nil *no-calls* nil)
	  (dolist (name names)
	    (multiple-value-bind (calls nested-calls time cons)
		(monitor-info-values name nested)
	      (declare (ignore nested-calls))
	      (when (minusp time) (setq time 0.0))
	      (when (minusp cons) (setq cons 0.0))
	      (if (zerop calls)
		  (push (if (symbolp name) 
			    (symbol-name name)
			    (format nil "~S" name))
			*no-calls*)
		  (push (make-monitoring-info
			 (format nil "~S" name) ; name
			 calls		; calls
			 (/ time (float time-units-per-second)) ; time in secs
			 (round cons)	; consing
			 (/ time (float total-time)) ; percent-time
			 (if (zerop total-cons) 0
			     (/ cons (float total-cons))) ; percent-cons
			 (/ (/ time (float calls)) ; time-per-call
			    time-units-per-second) ; sec/call
			 (round (/ cons (float calls)))) ; cons-per-call
			*monitor-results*))))
	  (display-monitoring-results threshold key ignore-no-calls)))))

(defun display-monitoring-results (&optional (threshold 0.01) (key :percent-time)
					     (ignore-no-calls t))
  (let ((max-length 8)			; Function header size
	(total-time 0.0)
	(total-consed 0)
	(total-calls 0)
	(total-percent-time 0)
	(total-percent-cons 0))			
    (sort-results key)
    (dolist (result *monitor-results*)
      (when (or (zerop threshold)
		(> (m-info-percent-time result) threshold))
	(setq max-length
	      (max max-length
		   (length (m-info-name result))))))
    (incf max-length 2)
    (format *trace-output*
	    "~&        ~VT                              Cons~
	     ~&        ~VT%     %                       Per      Total   Total~
	     ~&Function~VTTime  Cons  Calls  Sec/Call   Call     Time    Cons~
             ~&~V,,,'-A"
	    max-length max-length max-length
	    (+ max-length 53) "-")	
    (dolist (result *monitor-results*)
      (when (or (zerop threshold)
		(> (m-info-percent-time result) threshold))
	(format *trace-output* 
		"~&~A:~VT~,2F  ~,2F  ~5D  ~,6F  ~5D  ~,6F  ~6D"
		(m-info-name result)
		max-length
		(m-info-percent-time result)
		(m-info-percent-cons result)
		(m-info-calls result)
		(m-info-time-per-call result)
		(m-info-cons-per-call result)
		(m-info-time result)
		(m-info-cons result))
	(incf total-time (m-info-time result))
	(incf total-consed (m-info-cons result))
	(incf total-calls (m-info-calls result))
	(incf total-percent-time (m-info-percent-time result))
	(incf total-percent-cons (m-info-percent-cons result))))
    (format *trace-output* 
	    "~&~V,,,'-A~
	    ~&TOTAL:~VT~,2F  ~,2F  ~5D                   ~,6F  ~6D~
            ~&Estimated monitoring overhead: ~4,2F seconds~
            ~&Estimated total monitoring overhead: ~4,2F seconds"
	    (+ max-length 53) "-"
	    max-length 
	    total-percent-time total-percent-cons 
	    total-calls total-time total-consed
	    (/ (* *monitor-time-overhead* total-calls)
	       time-units-per-second)
	    *estimated-total-overhead*)
    (when (and (not ignore-no-calls) *no-calls*)
      (setq *no-calls* (sort *no-calls* #'string<))
      (let ((num-no-calls (length *no-calls*)))
	(if (> num-no-calls 20)
	    (format *trace-output*
		    "~%~@(~r~) monitored functions were not called. ~
                      ~%See the variable mon::*no-calls* for a list."
		    num-no-calls)
	    (format *trace-output*
		    "~%The following monitored functions were not called:~
                ~%~{~<~%~:; ~A~>~}~%"
		    *no-calls*))))
    (values)))

(defun sort-results (&optional (key :percent-time))
  (setq *monitor-results* 
	(case key
	  (:function             (sort *monitor-results* #'string>
				       :key #'m-info-name))
	  ((:percent-time :time) (sort *monitor-results* #'>
				       :key #'m-info-time))
	  ((:percent-cons :cons) (sort *monitor-results* #'>
				       :key #'m-info-cons))
	  (:calls	         (sort *monitor-results* #'>
				       :key #'m-info-calls))
	  (:time-per-call	 (sort *monitor-results* #'> 
				       :key #'m-info-time-per-call))
	  (:cons-per-call        (sort *monitor-results* #'>
				       :key #'m-info-cons-per-call)))))  

;;; *END OF FILE*

  
