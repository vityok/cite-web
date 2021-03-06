;;; -*- package: CITE-WEB; Syntax: Common-lisp; Base: 10 -*-

;;; завантаження вказаних статей та формування шаблону cite web для
;;; вікіпедії

;; (load "article-template.lisp")

(ql:quickload "drakma")
(ql:quickload "puri")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-interpol")
(ql:quickload "cxml")
(ql:quickload "flexi-streams")
(ql:quickload "hunchentoot")
(ql:quickload "cl-who")
(ql:quickload "cl-html-parse")
(ql:quickload "bordeaux-threads")

;;---------------------------------------------------------

(defpackage :cite-web
  (:use :cl)
  (:nicknames :cw)
  (:export :run-web-server))

(in-package :cite-web)

;;---------------------------------------------------------

;; use cl-interpol, i.e. the reader converts
;; #?" ${a} " to a string where the value of the symbol a is
;; substituted
(interpol:enable-interpol-syntax)

;;---------------------------------------------------------

(defvar *parser-alist* nil
  "Association list of domains and handlers defined by
DEFINE-EASY-PARSER")

;;---------------------------------------------------------

(defmacro define-easy-parser (uri name lambda-list &body body)
  "Макрос для визначення простих обробників доменів.

Приклад використання:

  (define-easy-parser \"домен.ком\" process-domain (url) тіло обробника)

Замість `url' можна вказувати інші назви для основного і єдиного
параметра - посилання, що слід обробити."
  `(progn
     (push (list ,uri ',name) *parser-alist*)
     (defun ,name ,lambda-list ,@body)))

;;---------------------------------------------------------

(defun dispatch-easy-parser (uri-host)
  "Шукає обробника для вказаного хоста. Повертає NIL якщо не вдалось
знайти обробника."
  (loop :for (uri easy-parser) :in *parser-alist*
     :when (search uri uri-host)
     :do (return easy-parser)))

;;---------------------------------------------------------

(defun make-cite-web (&key url title publisher date (author ""))
  (format nil "<ref>{{cite web
|url = ~a
|title = ~a
|author = ~a
|publisher = ~a
|date = ~a}}</ref>"
	  url
	  title
	  author
	  publisher
	  date))

;;---------------------------------------------------------

(defun get-meta (page meta)
  (let ((rxp (concatenate 'string "<meta name=\"" meta "\" content=\"([^\"]+)\"")))
    (ppcre:register-groups-bind (content)
	(rxp page)
      content)))

;;---------------------------------------------------------

(define-easy-parser "ukrinform.ua" process-ukrinform
  (url)
  (let* ((article-page (drakma:http-request url))
	 (article-title (ppcre:register-groups-bind (title)
						    (#?r"<h1>([^<]+)</h1>" article-page)
						    title))
	 (article-date (ppcre:register-groups-bind (date)
						   (#?r"<div class=\"date\"><span class=\"time\">[^<]+</span> (.+)</div>" article-page)
						   date)))
    (make-cite-web :url url
		   :title article-title
		   :publisher "УкрІнформ"
		   :date article-date)))

;;---------------------------------------------------------


(defun nil-resolver (pubid sysid)
  (declare (ignore pubid sysid))
  (flexi-streams:make-in-memory-input-stream nil))

;;---------------------------------------------------------

(define-easy-parser "online.wsj.com" process-wsj
  (url)
  (let* ((article-page (drakma:http-request url))
	 (article-title "")
	 (article-author "")
	 (article-date ""))

    (unwind-protect
	 (klacks:with-open-source
	     (s (cxml:make-source article-page
				  :entity-resolver #'nil-resolver))
	   (loop
	      for key = (klacks:peek s)
	      while key
	      do
	      (progn
		(case key
		  (:start-element
		   (if (string= "meta" (klacks:current-qname s))
		       (format t "~a~%" (klacks:list-attributes s))
		       )))
		(klacks:consume s)))))

    (make-cite-web :url url
		   :title article-title
		   :author article-author
		   :publisher "WSJ Online"
		   :date article-date)))

;;---------------------------------------------------------

(define-easy-parser "zeit.de" process-zeit (url)
  (let* ((article-page (drakma:http-request url))
	 (article-url url)
	 (article-title "")
	 (article-author "")
	 (article-date ""))

    (handler-case
	(klacks:with-open-source
	    (s (cxml:make-source article-page
				 :entity-resolver #'nil-resolver))
	  (loop
	     :for key = (klacks:peek s) :while key
	     :do (progn
		   (case key
		     (:start-element
		      (cond
			((string= "meta" (klacks:current-qname s))
			 (cond
			   ((string= "og:url" (klacks:get-attribute s "property"))
			    (setf article-url (klacks:get-attribute s "content")))

			   ((string= "og:title" (klacks:get-attribute s "property"))
			    (setf article-title (klacks:get-attribute s "content"))))))))
		   (klacks:consume s))))
      (condition () "failed to parse completely"))

    (setf article-date (ppcre:register-groups-bind (date)
			   (#?r"<strong>Datum</strong>\s*([^<]+) -" article-page)
			 date))

    (setf article-author (ppcre:register-groups-bind (author)
			     (#?r"<strong>Von</strong>\s*([^<]+)</" article-page)
			   author))

    (make-cite-web :url article-url
		   :title article-title
		   :author article-author
		   :publisher "Zeit Online"
		   :date article-date)))

;;---------------------------------------------------------

(define-easy-parser "wartime.org.ua" process-wartime
  (url)
  (let* ((article-page (drakma:http-request url))
	 (article-url url)
	 (article-title "")
	 (article-author "")
	 (article-date ""))

    (setf article-title (ppcre:register-groups-bind (title)
			    (#?r"<title>\s*([^<]+)\s*</title>" article-page)
			  title))

    (setf article-date (ppcre:register-groups-bind (date)
			   (#?r"  (\d+ \w+ \d{4}) \| Перегляди:\d+ \| Додати новину в закладки" article-page)
			 date))
    
    (make-cite-web :url article-url
		   :title article-title
		   :author article-author
		   :publisher "Військова панорама"
		   :date article-date)))

;;---------------------------------------------------------

(defun process-general (url)
  "Загальний оброник. На випадок, якщо спеціалізованого обробника
нема."

  (let* ((article-page (drakma:http-request url))
	 (article-url url)
	 (article-title "")
	 (article-author "")
	 (article-date ""))

    (setf article-title (ppcre:register-groups-bind (title)
			    (#?r"<title>\s*([^<]+)\s*</title>" article-page)
			  title))
    
    (make-cite-web :url article-url
		   :title article-title
		   :author article-author
		   :publisher (puri:URI-HOST (puri:URI url))
		   :date article-date)))

;;---------------------------------------------------------

(defun dump-article-text (url &optional (out #P"article.txt"))
  #+sbcl
  (sb-ext:run-program "/usr/bin/w3m"
		      `("-dump"
			,url)
		      :output out
		      :if-output-exists :supersede)

  #+ccl
  (ccl:run-program "/usr/bin/w3m"
		   `("-dump"
		     ,url)
		   :output out
		   :if-output-exists :supersede))

;;---------------------------------------------------------

(defun process-article-url (url)
  "Тут виконується перенаправлення на конкретну фукнцію-обробник для
посилання."
  (let* ((news-host (puri:URI-HOST (puri:URI url)))
	 (easy-parser (dispatch-easy-parser news-host)))
    (if easy-parser
	(funcall easy-parser url)
	(process-general url))))

;;---------------------------------------------------------

(defun pa (url)
  "Скорочений псевдонім для основної функції."
  (process-article-url url))

;;---------------------------------------------------------
;; Інтерфейс користувача
;;---------------------------------------------------------

(defmacro with-html (&body body)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t)
     (setf who:*attribute-quote-char* #\")
     ,@body))

;;---------------------------------------------------------

(tbnl:define-easy-handler (h-about :uri "/about.html"
				   :default-request-type :get)
    ()
  (with-html
      (:html
       (:head
	(:title "Про Wikipedia Cite web template generator"))
       (:body
	(:p "“Wikipedia Cite web template generator” дозволяє швидко та зручно заповнювати шаблон "
	    (:a :href "http://uk.wikipedia.org/wiki/Шаблон:Cite_web" "Cite web")
	    " для вказаних інтернет-ресурсів.")
	(:p "Програмні коди програми можна переглянути на сторінці проекту "
	    (:a :href "https://github.com/vityok/cite-web" "на сайті GitHub"))
	(:p (:a :href "article.html" "Повернутись на головну сторінку"))))))

;;---------------------------------------------------------

(tbnl:define-easy-handler (h-article-template :uri "/article.html"
					      :default-request-type :get)
    ((url :parameter-type 'string))
  (with-html
      (:html
       (:head
	(:title "Wikipedia Cite web template generator"))
       (:body
	(:form :method :get :action "/article.html"
	       (:span "Адреса статті: ")
	       (:input :type :text :name "url" :size 50 :value url)
	       (:input :type :submit))
	(when url
	  (who:htm
	   (:p "Шаблон для Вікіпедії:")
	   (:pre
	    (who:esc (process-article-url url)))))
	(:p (:a :href "about.html" "Про програму"))))))

;;---------------------------------------------------------

(defun run-web-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4141))
  (format *standard-output* "Now it is time to visit http://localhost:4141/article.html~%")  
  ;; the acceptor runs in a separate thread. In order to prevent
  ;; premature cite-web exit join all threads. Even if execution does
  ;; not go past this cycle it still will keep the server alive
  (dolist (thread (bt:all-threads))
    (bt:join-thread thread)))

;;---------------------------------------------------------

(format *standard-output* "Evaluate (run-web-server) to start serving requests on the web~%")

;; EOF
