(module speech-dispatcher-client

(;; response record
 response?
 response-code
 response-data
 response-status

 ;; connection record
 connection?
 connection-type
 connection-path
 connection-host
 connection-port
 connection-inport
 connection-outport

 ;; Parameters
 enable-debug?
 debug-to

 ;; Procedures
 connect

 ;; Protocol
 speak
 list-voices
 list-output-modules
 list-synthesis-voices
 key
 stop
 cancel
 pause
 resume
 set-client-name
 set-output-module
 set-language
 set-ssml-mode
 set-punctuation
 set-spelling
 set-capital-letters-recognition
 set-voice
 set-synthesis-voice
 set-rate
 set-pitch
 set-volume
 set-pause-context
 set-history
 set-self-notification-all
 set-self-notification-begin
 set-self-notification-end
 set-self-notification-cancel
 set-self-notification-pause
 set-self-notification-resume
 set-self-notification-index-marks
 set-debug
 set-priority
 block-begin
 block-end
 history-get-client-list
 history-get-client-id
 history-get-client-messages
 history-get-last
 history-get-message
 history-get-cursor
 history-set-cursor
 history-cursor
 history-say
 history-sort
 history-set-short-message-length
 history-set-message-type-ordering
 history-search
 quit
 help
 )

(import (rename chicken (quit chicken-quit)) scheme)
(use data-structures extras ports posix tcp)
(use unix-sockets)

(define enable-debug?
  (make-parameter #f))

(define debug-to
  (make-parameter (current-error-port)))

(define (debug fmt . args)
  (when (enable-debug?)
    (with-output-to-port (debug-to)
      (lambda ()
        (apply printf
               (cons (string-append
                      "[" (time->string (seconds->local-time)) "] "
                      fmt
                      "\n")
                     args))))))

(define-record connection type path host port inport outport)

(define-record-printer (connection obj out)
  (if (eq? (connection-type obj) 'tcp)
      (fprintf out
               "#<connection ~S ~a>"
               (connection-host obj)
               (connection-port obj))
      (fprintf out
               "#<connection ~S>"
               (connection-path obj))))

(define-record response code data status)

(define-record-printer (response obj out)
  (fprintf out
           "#<response ~a ~S ~S>"
           (response-code obj)
           (response-status obj)
           (response-data obj)))

(define (connect #!key path host port)
  (when (and path host)
    (error 'connect "Only one of path or host should be provided"))
  (when (and host (not port))
    (error 'connect "Missing port"))
  (let ((type (if port 'tcp 'unix)))
    (let-values (((in out) (if (eq? type 'unix)
                               (unix-connect path)
                               (tcp-connect host port))))
      (make-connection type path host port in out))))

(define (read-response conn)
  (let ((parsed-resp (make-response #f '() "")))
    (let loop ()
      (let ((resp (handle-exceptions exn
                    (signal exn)
                    (read-line (connection-inport conn)))))
        (if (and resp (> (string-length resp) 2))
            (let* ((code (string->number (substring resp 0 3)))
                   (rest (substring resp 3)) ;; 3 digits
                   (data (substring-index "-" rest)))
              (assert code)
              (if (and data (zero? data))
                  (begin
                    (response-data-set! parsed-resp
                                        (cons (cons code (substring rest 1))
                                              (response-data parsed-resp)))
                    (loop))
                  (begin
                    (response-code-set! parsed-resp code)
                    (response-status-set! parsed-resp (substring rest 1))
                    (response-data-set! parsed-resp
                                        (reverse (response-data parsed-resp)))
                    (debug "Response: ~S" parsed-resp)
                    parsed-resp)))
            (error 'read-response "Invalid response" resp))))))

(define (send-data! conn data)
  (let ((port (connection-outport conn)))
    (display (string-append data "\r\n.\r\n") port)
    (flush-output port)))

(define (send! conn data)
  (let ((port (connection-outport conn)))
    (display (string-append data "\r\n") port)
    (flush-output port)))


;;; Checkers
(define (check-self/all/id loc thing)
  (unless (or (integer? thing) (memq thing '(all self)))
    (error loc "Invalid argument" thing)))

(define (check-positive-integer loc n var)
  (##sys#check-integer n loc)
  (when (fx< n 0)
    (error loc "Expected positive integer for " var)))

(define (check-status loc status)
  (unless (memq status '(on off))
    (error loc "Invalid status" status)))

;;;
;;; Protocol
;;;

;;; SPEAK
(define (speak conn text)
  (send! conn "SPEAK")
  (read-response conn)
  (send-data! conn text)
  (read-response conn))

;;; LIST
(define (list-voices conn)
  (send! conn "LIST VOICES")
  (read-response conn))

(define (list-output-modules conn)
  (send! conn "LIST OUTPUT_MODULES")
  (read-response conn))

(define (list-synthesis-voices conn)
  (send! conn "LIST SYNTHESIS_VOICES")
  (read-response conn))

;;; KEY
(define (key conn key-name)
  (send! conn (string-append "KEY " key-name))
  (read-response conn))

;;; STOP
(define (stop conn thing)
  (check-self/all/id 'stop thing)
  (send! conn (conc "STOP " thing))
  (read-response conn))

;;; CANCEL
(define (cancel conn thing)
  (check-self/all/id 'cancel thing)
  (send! conn (conc "CANCEL " thing))
  (read-response conn))

;;; PAUSE
(define (pause conn thing)
  (check-self/all/id 'pause thing)
  (send! conn (conc "PAUSE " thing))
  (read-response conn))

;;; RESUME
(define (resume conn thing)
  (check-self/all/id 'resume thing)
  (send! conn (conc "RESUME " thing))
  (read-response conn))

;;; SET
(define (set-client-name conn client-name)
  (send! conn (conc "SET self CLIENT_NAME " client-name))
  (read-response conn))

(define (set-output-module conn thing module)
  (check-self/all/id 'set-output-module thing)
  (send! conn (conc "SET " thing " OUTPUT_MODULE " module))
  (read-response conn))

(define (set-language conn thing language)
  (check-self/all/id 'set-language thing)
  (send! conn (conc "SET " thing " LANGUAGE " language))
  (read-response conn))

(define (set-ssml-mode conn mode)
  (send! conn (conc "SET self SSML_MODE " mode))
  (read-response conn))

(define (set-punctuation conn thing mode)
  (check-self/all/id 'set-language thing)
  (unless (memq mode '(all some none))
    (error 'set-punctuation "Invalid mode" mode))
  (send! conn (conc "SET " thing " PUNCTUATION " mode))
  (read-response conn))

(define (set-spelling conn thing status)
  (check-self/all/id 'set-spelling thing)
  (check-status 'set-spelling status)
  (send! conn (conc "SET " thing " SPELLING " status))
  (read-response conn))

(define (set-capital-letters-recognition conn thing mode)
  (check-self/all/id 'set-capital-letters-recognition thing)
  (unless (memq mode '(none spell icon))
    (error 'set-capital-letters-recognition "Invalid mode" mode))
  (send! conn (conc "SET " thing " CAP_LET_RECOGN " mode))
  (read-response conn))

(define (set-voice conn thing name)
  (check-self/all/id 'set-voice thing)
  (send! conn (conc "SET " thing " VOICE " name))
  (read-response conn))

(define (set-synthesis-voice conn thing name)
  (check-self/all/id 'set-synthesis-voice thing)
  (send! conn (conc "SET " thing " SYNTHESIS_VOICE " name))
  (read-response conn))

(define (set-rate conn thing n)
  (##sys#check-range n -100 101 'set-rate)
  (check-self/all/id 'set-synthesis-voice thing)
  (send! conn (conc "SET " thing " RATE " n))
  (read-response conn))

(define (set-pitch conn thing n)
  (##sys#check-range n -100 101 'set-pitch)
  (check-self/all/id 'set-synthesis-voice thing)
  (send! conn (conc "SET " thing " PITCH " n))
  (read-response conn))

(define (set-volume conn thing n)
  (##sys#check-range n -100 101 'set-volume)
  (check-self/all/id 'set-synthesis-voice thing)
  (send! conn (conc "SET " thing " VOLUME " n))
  (read-response conn))

(define (set-pause-context conn thing n)
  (check-positive-integer 'set-pause-context n 'n)
  (check-self/all/id 'set-synthesis-voice thing)
  (send! conn (conc "SET " thing " PAUSE_CONTEXT " n))
  (read-response conn))

(define (set-history conn thing status)
  (check-self/all/id 'set-history thing)
  (check-status 'set-history status)
  (send! conn (conc "SET " thing " HISTORY " status))
  (read-response conn))

;;; Notifications
(define (set-self-notification-all conn status)
  (send! conn (conc "SET SELF NOTIFICATION ALL " status))
  (read-response conn))

(define (set-self-notification-begin conn status)
  (check-status 'set-self-notification-begin status)
  (send! conn (conc "SET SELF NOTIFICATION BEGIN " status))
  (read-response conn))

(define (set-self-notification-end conn status)
  (check-status 'set-self-notification-end status)
  (send! conn (conc "SET SELF NOTIFICATION END " status))
  (read-response conn))

(define (set-self-notification-cancel conn status)
  (check-status 'set-self-notification-cancel status)
  (send! conn (conc "SET SELF NOTIFICATION CANCEL " status))
  (read-response conn))

(define (set-self-notification-pause conn status)
  (check-status 'set-self-notification-pause status)
  (send! conn (conc "SET SELF NOTIFICATION PAUSE " status))
  (read-response conn))

(define (set-self-notification-resume conn status)
  (check-status 'set-self-notification-resume status)
  (send! conn (conc "SET SELF NOTIFICATION RESUME " status))
  (read-response conn))

(define (set-self-notification-index-marks conn status)
  (check-status 'set-self-notification-index-marks status)
  (send! conn (conc "SET SELF NOTIFICATION INDEX_MARKS " status))
  (read-response conn))

(define (set-debug conn status)
  (check-status 'set-debug status)
  (send! conn (conc "SET all DEBUG " status))
  (read-response conn))

(define (set-priority conn priority)
  (unless (memq priority '(important text message notification progress))
    (error 'set-priority "Invalid priority" priority))
  (send! conn (conc "SET self PRIORITY " priority))
  (read-response conn))

;; Blocks
(define (block-begin conn)
  (send! conn "BLOCK BEGIN")
  (read-response conn))

(define (block-end conn)
  (send! conn "BLOCK END")
  (read-response conn))

;;; HISTORY
(define (history-get-client-list conn)
  (send! conn "HISTORY GET CLIENT_LIST")
  (read-response conn))

(define (history-get-client-id conn)
  (send! conn "HISTORY GET CLIENT_ID")
  (read-response conn))

(define (history-get-client-messages conn thing start number)
  (check-positive-integer 'history-get-client-messages start 'start)
  (check-positive-integer 'history-get-client-messages number 'number)
  (check-self/all/id 'set-synthesis-voice thing)
  (send! conn (conc "HISTORY GET CLIENT_MESSAGES " thing  start " " number))
  (read-response conn))

(define (history-get-last conn)
  (send! conn "HISTORY GET LAST")
  (read-response conn))

(define (history-get-message conn message-id)
  (check-positive-integer 'history-get-message message-id 'message-id)
  (send! conn "HISTORY GET LAST")
  (read-response conn))

(define (history-get-cursor conn)
  (send! conn "HISTORY CURSOR GET")
  (read-response conn))

(define (history-set-cursor conn thing pos #!optional n)
  (check-self/all/id 'history-cursor-set! thing)
  (when (eq? pos 'pos)
    (check-positive-integer 'history-cursor-set n 'n))
  (send! conn (conc "HISTORY CURSOR SET " thing " " pos
                    (if n (conc " " n) "")))
  (read-response conn))

(define (history-cursor conn direction)
  (unless (memq direction '(forward backward))
    (error 'history-cursor "Invalid direction" direction))
  (send! conn (conc "HISTORY CURSOR " direction))
  (read-response conn))

(define (history-say conn id)
  (check-positive-integer 'history-say id 'id)
  (send! conn (conc "HISTORY SAY " id))
  (read-response conn))

(define (history-sort conn order criteria)
  (unless (memq order '(asc desc))
    (error 'history-sort "Invalid order" order))
  (unless (memq criteria '(time user client_name priority message_type))
    (error 'history-sort "Invalid criteria" criteria))
  (send! conn (conc "HISTORY SORT " order " " criteria))
  (read-response conn))

(define (history-set-short-message-length conn length)
  (check-positive-integer 'history-set-short-message-length length 'length)
  (send! conn (conc "HISTORY SET SHORT_MESSAGE_LENGTH " length))
  (read-response conn))

(define (history-set-message-type-ordering conn ordering)
  (for-each
   (lambda (order)
     (unless (memq order '(text sound_icon char key))
       (error 'history-set-message-type-ordering
              "Invalid ordering" order)))
   ordering)
  (send! conn (sprintf "HISTORY SET MESSAGE_TYPE_ORDERING \"~a\""
                       (string-intersperse (map ->string ordering))))
  (read-response conn))

(define (history-search conn thing condition)
  (check-self/all/id 'history-search thing)
  (send! conn (sprintf "HISTORY SEARCH ~a \"~a\"" thing condition))
  (read-response conn))

;;; QUIT
(define (quit conn)
  (send! conn "QUIT")
  (read-response conn))

;;; HELP
(define (help conn)
  (send! conn "HELP")
  (read-response conn))

) ;; end module
