(defpackage #:ewmh
  (:use :cl))
(in-package #:ewmh)

(defparameter *request-event-mask* (xlib:make-event-mask ))

(defparameter *orientation*
  '((:horizontal . 0)
    (:vertical . 1)))
(defparameter *corner*
  '((:top-left . 0)
    (:top-right . 1)
    (:bottom-right . 2)
    (:bottom-left . 3)))

;;;; Utils
(defun layout-octets (layout)
  (let ((orientation (cdr (assoc (first layout) *orientation*)))
	(corner (cdr (assoc (fourth layout) *corner*))))
    `(,orientation ,@(subseq layout 1 3) ,corner)))

(defun octets-layout (octets)
  (let ((orientation (car (rassoc (first octets) *orientation*)))
	(corner (car (rassoc (fourth octets) *corner*))))
    `(,orientation ,@(subseq octets 1 3) ,corner)))

(defun utf8->null-terminated-octets (string)
  (append (coerce (babel:string-to-octets string :encoding :utf-8) 'list) (list 0)))
(defun octets->utf8-strings (list)
  (let* ((vectors  (mapcar (lambda (seq) (coerce seq '(vector (unsigned-byte 8)))) (remove nil (split-sequence:split-sequence 0 list))))
	 (list? (mapcar  (lambda (seq) (babel:octets-to-string seq :encoding :utf-8)) vectors)))
    ;; should we be making this list not list decision here, or let the client always expect a list?
    (if (= 1 (length list?))
	(car list?)
	list?)))

(export 'property)
(defun property (window property)
  (multiple-value-bind (data type reply-format bytes-after) (xlib:get-property window property :result-type 'list) 
    (declare (ignore reply-format bytes-after))
    (case type
      (:utf8_string (octets->utf8-strings data))
      ((:string :compound_text) (concatenate 'string (mapcar #'code-char data)));fixme add real compound text handling
      (:atom (if (= 1 (length data)) ;same list not list question, so obviously this is not where we deal with it.
		 (xlib:atom-name (xlib:window-display window) (car data))
      		 (mapcar (lambda (i) (xlib:atom-name (xlib:window-display window) i)) data)))
      (:window (mapcar (lambda (xid) (xlib::lookup-window (xlib:window-display window) xid)) data))
      (:cardinal data)
      (t (values data type)))))

(eval-when (:compile-toplevel)
  (defun lispify (keyword &optional (type :regular))
    (let* ((string (case type
		     (:regular (symbol-name keyword))
		     (:set (concatenate 'string "SET-" (symbol-name keyword)))
		     (:request (concatenate 'string (symbol-name keyword) "-REQUEST"))))
	   (position (position #\M string))
	   (start (or (and position (= 6 position) 8)5)))
      (intern (string-upcase (substitute #\- #\_ (subseq string start))) (find-package 'ewmh)))))

(defmacro defwmh (keyword window setarg size type &key set-transform pre-transform post-transform)
  (let* ((name (lispify keyword))
	 (set-name (lispify keyword :set)))
    `(progn
       (export ',name)
       (export ',set-name)
       (defun ,name (,window)
	 (let ((data (property ,window ,keyword)))
	   (if ,post-transform
	       (funcall ,post-transform data)
	       data)))
       (defun ,set-name (,window ,setarg)
	 (let* ((dat (if ,pre-transform
			  (funcall ,pre-transform ,setarg)
			  ,setarg))
		(data (if (listp dat)
			 dat
			 (list dat))))
	   (xlib:change-property ,window ,keyword data ,type ,size :transform ,set-transform)))
       (defsetf ,name ,set-name))))

;;;; root-window properties
(defwmh :_net_supported root-window atom-list 32 :atom :set-transform (lambda (number) (xlib:find-atom (xlib:window-display root-window) number)))
(defwmh :_net_client_list root-window window-list 32 'xlib:window :set-transform (lambda (win) (xlib:window-id win)))
(defwmh :_net_client_list_stacking root-window window-list 32 'xlib:window :set-transform (lambda (win) (xlib:window-id win)))
(defwmh :_net_number_of_desktops root-window number 32 :cardinal)
(defwmh :_net_desktop_geometry root-window width-height-list 32 :cardinal)
(defwmh :_net_desktop_viewport root-window width-height-list 32 :cardinal)
(defwmh :_net_current_desktop root-window number 32 :cardinal)
(defwmh :_net_desktop_names root-window utf8-name-list 8 :utf8_string :pre-transform (lambda (list) (mapcan #'utf8->null-terminated-octets list)))
(defwmh :_net_active_window root-window active-window 32 'xlib:window :set-transform (lambda (win) (xlib:window-id win)))
(defwmh :_net_workarea root-window lists-of-x-y-width-height 32 :cardinal :pre-transform (lambda (list) (apply 'append list))) ;add group function?
(defwmh :_net_supporting_wm_check root-or-child-window child-window 32 'xlib:window :set-transform (lambda (win) (xlib:window-id win)))
(defwmh :_net-virtual_roots root-window windows 32 'xlib:window :set-transform (lambda (win) (xlib:window-id win)))
(defwmh :_net_desktop_layout root-window layout-list 32 :cardinal :pre-transform #'layout-octets :post-transform #'octets-layout)
(defwmh :_net_showing_desktop root-window showing 32 :cardinal)

;;;; application window properties
(defwmh :_net_wm_name window utf8-string 8 :utf8_string :pre-transform #'utf8->null-terminated-octets)
(defwmh :_net_wm_visible_name window utf8-string 8 :utf8_string :pre-transform #'utf8->null-terminated-octets)
(defwmh :_net_wm_icon_name window utf8-string 8 :utf8-string :pre-transform #'utf8->null-terminated-octets)
(defwmh :_net_wm_visible_icon_name window utf8-string 8 :utf8-string :pre-transform #'utf8->null-terminated-octets)
(defwmh :_net_wm_desktop window desktop 32 :cardinal)
(defwmh :_net_wm_window_type window atom-list 32 :atom :set-transform (lambda (number) (xlib:find-atom (xlib:window-display window) number)))
(defwmh :_net_wm_state window atom-list 32 :atom :set-transform (lambda (number) (xlib:find-atom (xlib:window-display window) number)))
(defwmh :_net_wm_allowed_actions window atom-list 32 :atom :set-transform (lambda (number) (xlib:find-atom (xlib:window-display window) number)))
(defwmh :_net_wm_strut window left-right-top-bottom 32 :cardinal)
(defwmh :_net_wm_strut_partial window complicated-crap 32 :cardinal) ;this should probably be a case for &key args maybe struct? rewrite?
(defwmh :_net_wm_icon_geometry window geometry 32 :cardinal)
(defwmh :_net_wm_icon window packed-icon 32 :cardinal)
(defwmh :_net_wm_pid window pid 32 :cardinal)
(defwmh :_net_wm_handled_icons window handled? 32 :cardinal)
(defwmh :_net_wm_user_time window xserver-time 32 :cardinal)
(defwmh :_net_frame_extents window extents 32 :cardinal)

;;;; Request messages 

(defmacro defwmr (keyword sender receiver &rest args)
  (let ((name (lispify keyword :request)))
    `(progn
       (export ',name)
       (defun ,name (,sender ,receiver ,@args )
	 (xlib:send-event ,receiver :client-message '(:substructure-notify :substructure-redirect) :window ,sender :propagate-p nil :type ,keyword :format 32 :data (list ,@args))))))

(defwmr :_net_number_of_desktops sender root-window number)
(defwmr :_net_desktop_geometry pager root-window width height)
(defwmr :_net_desktop_viewport pager root-window x y)
(defwmr :_net_current_desktop pager root-window desktop timestamp)
(defwmr :_net_active_window window-to-activate root-window source timestamp requestors-active-window);windows need to be transformed to xids
(defwmr :_net_showing_desktop pager root-window c-boolean )
(defwmr :_net_close_window window root-window timestamp source)
(defwmr :_net_moveresize_window window root-window source x y width height);add transform?
(defwmr :_net_moveresize window root-window x y direction button source ); find wm that supports this to test how it works
(defwmr :_net_restack_window window root-window source sibling detail)
(defwmr :_net_request_frame_extents window root-window); handle separately so 'request' isn't doubled in name?
(defwmr :_net_wm_desktop window root-window desktop source)
(defwmr :_net_wm_state window root-window action property1 property2 source)

;;;; Window Manager Protocols
 (defun ping (client wm timestamp)
   (xlib:send-event wm :client-message '(:substructure-notify :substructure-redirect)
		       :window client
		       :propagate-p nil
		       :type :wm_protocols
		       :format 32
		       :data (list (xlib:find-atom (xlib:window-display wm) :_net_wm_ping) timestamp (xlib:window-id client))))

(defun sync-request(client root-window timestamp request-number)
  (let ((low (ldb (byte 32 0) request-number))
	(high (ldb (byte 32 32) request-number)));31 or 32? test
    (xlib:send-event root-window :client-message '(:substructure-notify :substructure-redirect) :window client :propagate-p nil :type :wm_protocols :format 32
			    :data (list (xlib:find-atom (xlib:window-display client) :_net_wm_sync_request) timestamp low high))))
