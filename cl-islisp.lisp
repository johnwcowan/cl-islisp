(cl:in-package :common-lisp-user)

(cl:defpackage :islisp
  (:import-from :common-lisp
    ; 12.7 Functions
      :functionp :function :lambda :labels :flet :apply :funcall
    ; 12.8 Defining operators
      :defun
    ; 13.1 Boolean values
      :t :nil
    ; 13.2 Equality
      :eq :eql :equal
    ; 13.3 Logical connectives
      :not :and :or
    ; 14.1 Constants
      :quote
    ; 14.2 Variables
      :setq :setf :let :let*
    ; 14.4 Conditional expressions
      :if :cond :case
    ; 14.5 Sequencing forms
      :progn
    ; 14.7 Non-local exits
      :block :return-from :catch :throw :tagbody go unwind-protect
    ; 17 Declarations and coercions
      :the
    ; 18 Symbol class
      :symbolp :gensym
    ; 19.1 Number class
      :numberp := :>= :<= :> :< :+ :* :-
      :max :min :abs :exp :log :expt :sqrt :sin :cos :tan :atan
      :sinh :cosh :tanh :atanh
    ; 19.2 Float class
      :floatp :float
    ; 19.2 Integer class
      :integerp :mod :gcd :lcm :isqrt
    ; 20 Character class
      :characterp :char= :char/= :char< :char> :char<= :char>=
    ; 21.1 Cons
      :consp :cons :car :cdr
    ; 21.2 Null class
      :null
    ; 21.3 List operations
      :listp :list :reverse :nreverse :member
      :mapcar :mapc :mapcan :maplist :mapl :mapcon :assoc
    ; 22.3 Array operations
      :aref :array-dimensions
    ; 23 Vectors
      :vector
    ; 24 String class
      :stringp
      :string= :string/= :string< :string> :string<= :string>=
    ; 25 Sequence functions
      :length :elt :subseq
    ; 26 Stream class
      :streamp :open-stream-p :input-stream-p :output-stream-p
    ; 26.1 Streams to files
      :close :finish-output
    ; 27.2 Character I/O
      :read :read-char :read-line
      :format
    ; 27.3 Binary I/O
      :read-byte :write-byte
    ; 28 Files
      :probe-file :file-position :file-length
    ; 29.2 Signaling and handling conditions
      :error :cerror :ignore-errors
    ; 29.3 Data associated with condition classes
      :arithmetic-error-operation :arithmetic-error-operands
      :stream-error-stream
    ; 30 Miscellaneous
      :identity :get-universal-time
      :get-internal-run-time :get-internal-real-time
      :internal-time-units-per-second)

  (:export
    ; 10.2 Predefined classes
      :<arithmetic-error> :<basic-array> :<basic-array*> :<basic-vector>
      :<character> :<cons> :<control-error> :<division-by-zero> :<domain-error>
      :<end-of-stream> :<error> :<float> :<floating-point-overflow>
      :<floating-point-underflow> :<function> :<general-array> :<general-array*>
      :<general-vector> :<generic-function> :<integer> :<list> :<null> :<number>
      :<object> :<parse-error> :<program-error> :<serious-condition> :<simple-error>
      :<standard-generic-function> :<standard-object> :<storage-exhausted>
      :<stream> :<stream-error> :<string> :<symbol> :<unbound-variable>
      :<undefined-entity> :<undefined-function>
    ; 12.7 Functions
      :functionp :function :lambda :labels :flet :apply :funcall
    ; 12.8 Defining operators
      :defconstant :defglobal :defdynamic :defun
    ; 13.1 Boolean values
      :t :nil
    ; 13.2 Equality
      :eq :eql :equal
    ; 13.3 Logical connectives
      :not :and :or
    ; 14.1 Constants
      :quote
    ; 14.2 Variables
      :setq :setf :let :let*
    ; 14.3 Dynamic variables
      :dynamic :set-dynamic :dynamic-let
    ; 14.4 Conditional expressions
      :if :cond :case :case-using
    ; 14.5 Sequencing forms
      :progn
    ; 14.6 Iteration
      :while :for
    ; 14.7 Non-local exits
      :block :return-from :catch :throw :tagbody :go :unwind-protect
    ; 15.1 Defining classes
      :defclass
    ; 15.2 Generic functions
      :generic-function-p :defgeneric :defmethod
    ; 15.4 Object creation and initialization
      :create :initialize-object
    ; 15.5 Class enquiry
      :class-of :instancep :subclassp :class
    ; 17 Declarations and coercions
      :the :assure
    ; 18 Symbol class
      :symbolp :property :set-property :remove-property :gensym
    ; 19.1 Number class
      numberp :parse-number := :/= :>= :<= :> :< :+ :* :- :quotient :reciprocal
      :max :min :abs :exp :log :expt :sqrt :*pi* :sin :cos :tan :atan :atan2
      :sinh :cosh :tanh :atanh
    ; 19.2 Float class
      :*most-positive-float :*most-negative-float* :floatp :float
      :floor :ceiling :truncate :round
    ; 19.2 Integer class
      :integerp :div :mod :gcd :lcm :isqrt
    ; 20 Character class
      :characterp :char= :char/= :char< :char> :char<= :char>=
    ; 21.1 Cons
      :consp :cons :car :cdr :set-car :set-cdr
    ; 21.2 Null class
      :null
    ; 21.3 List operations
      :listp :create-list :list :reverse :nreverse :member
      :mapcar :mapc :mapcan :maplist :mapl :mapcon :assoc
    ; 22.3 Array operations
      :basic-array-p :basic-array*-p :general-array*-p
      :create-array :aref :garef :set-aref :set-garef :array-dimensions
    ; 23 Vectors
      :basic-vector-p :general-vector-p :create-vector :vector
    ; 24 String class
      :stringp :create-string
      :string= :string/= :string< :string> :string<= :string>=
      :char-index :string-index :string-append
    ; 25 Sequence functions
      :length :elt :set-elt :subseq :map-into
    ; 26 Stream class
      :streamp :open-stream-p :input-stream-p :output-stream-p
      :standard-input :standard-output :error-output
      :with-standard-input :with-standard-output :with-error-output
    ; 26.1 Streams to files
      :open-input-file :open-output-file :open-io-file
      :with-open-input-file :with-open-output-file :with-open-io-file
      :close :finish-output
    ; 26.2 Other streams
      :create-string-input-stream :create-string-output-stream
      :get-output-stream-string
    ; 27.2 Character I/O
      :read :read-char :preview-char :read-line :stream-ready-p
      :format :format-char :format-float :format-fresh-line
      :format-integer :format-object :format-tab
    ; 27.3 Binary I/O
      :read-byte :write-byte
    ; 28 Files
      :probe-file :file-position :set-file-position :file-length
    ; 29.2 Signaling and handling conditions
      :error :cerror :signal-condition :ignore-errors :report-condition
      :condition-continuable :continue-condition :with-handler
    ; 29.3 Data associated with condition classes
      :arithmetic-error-operation :arithmetic-error-operands
      :domain-error-object :domain-error-expected-class
      :parse-error-string :parse-error-expected-class
      :simple-error-format-string :simple-error-format-arguments
      :stream-error-stream
      :undefined-entity-name :undefined-entity-namespace
    ; 30 Miscellaneous
      :identity :get-universal-time
      :get-internal-run-time :get-internal-real-time
      :internal-time-units-per-second))

(cl:defpackage :islisp-user
  (:use :islisp))

(cl:defpackage :islisp-impl
  (:use :common-lisp))

(cl:defpackage :islisp-dynamic
  (:use))

(cl:load "defglobal.lisp")
(cl:load "parse-number.lisp")
(cl:load "clause18-21.lisp")
