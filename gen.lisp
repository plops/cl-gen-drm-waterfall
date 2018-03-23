(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro er (&body body)
  `(statements (<< "std::cerr" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro checked-ioctl (fd cmd arg)
  `(if (< (funcall ioctl ,fd ,cmd ,arg) 0)
     (macroexpand (er ,(format nil "ioctl ~a failed." cmd)))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun dox (&key brief usage params return)
  `(
    (raw " ")
    (raw ,(format nil "//! @brief ~a" brief)) (raw "//! ")
    (raw ,(format nil "//! @usage ~a" usage)) (raw "//! ")
    ,@(loop for (name desc) in params collect
         `(raw ,(format nil "//! @param ~a ~a" name desc)))
    (raw "//! ")
    (raw ,(format nil "//! @return ~a" return))
    (raw " ")))


(defparameter *drm-facts*
  `((10 "")))


(defmacro with-open-fstream ((f fn &key (dir "/dev/shm")) &body body)
  `(let ((,f :type "std::ofstream" :ctor (comma-list (string ,(format nil "~a/~a" dir fn)))))
     ,@body))


(defmacro benchmark (&body body)
  `(let ((start :ctor (funcall current_time)))
     ,@body
     (macroexpand (e "time: " (* (/ 1.0 1000.0) (- (funcall current_time) start)) " ms"))
     ))
(defun rev (x nn)
  (let ((n (floor (log nn 2)))
      (res 0))
  (dotimes (i n)
    (setf (ldb (byte 1 i) res) (ldb (byte 1 (- n 1 i)) x)))
  res))


(progn
  (let* ((n 4192)
	 (code `(with-compilation-unit
		    (with-compilation-unit
			(raw "//! \\file main.cpp Draw to screen using linux direct rendering manager"))

		  (include <iostream>)
		  (include <cassert>)
		  (include <cstdlib>)
		  (include <errno.h>)
		  (include <cstring>)
		  (include <sys/mman.h>)
		  (include <unistd.h>)

		  (raw " ")
		  (include <xf86drm.h>)
		  (include <xf86drmMode.h>)
		  (include <i915_drm.h>)

		  (raw " ")
		  (include <iio.h>)

		  (raw " ")
		  (include <cmath>)
		  (include <algorithm>)
		  (include <array>)
		  (include <complex>)
		  (include <sys/time.h>) ;; gettimeofday
		  
		  (raw "//! This repository contains a minimal program to draw to a linux screen.")
		  (raw "//! \\section Dependencies ")
		  (raw "//! - Linux kernel with DRM driver")
		  (raw "//! - libdrm")
		  (raw " ")
		  (raw "//! - sbcl to generate c++ code")
					;(raw "//! - cmake to configure for build")
		  (raw "//! - g++ to compile c++ code")
		  (raw " ")
		  (raw "//! - For the documentation (optional):")
		  (raw "//!   + doxygen")
		  (raw " ")
		  
		  (raw " ")
		  (raw "//! \\section References ")
		  ,@(loop for i from 1 and e in '("http://betteros.org/tut/graphics1.php")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))

		  (enum Constants (M_MAG_N ,n))

		  (decl ((m_fft_in :type "std::array<std::complex<float>,M_MAG_N>"  :init (list (list ,@ (loop for i below n collect 0.0))))
			 (m_fft_out :type "std::array<std::complex<float>,M_MAG_N>"  :init (list (list ,@ (loop for i below n collect 0.0))))
			 #+nil (m_fft_out2 :type "std::array<std::complex<float>,M_MAG_N>"  :init (list (list ,@ (loop for i below n collect 0.0))))
			 (m_fft_out_mag :type "std::array<float,M_MAG_N>" :init (list (list ,@ (loop for i below n collect 0.0))))
			 ))
		  (function (current_time () "static inline uint64_t")
			    
			    (let ((tv :type "struct timeval"))
			      (funcall gettimeofday &tv nullptr)
			      (return (+ (* tv.tv_sec 1000000)
					 tv.tv_usec))))
		  
		  ;; https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm
		  (function  (bit_reverse_copy ((in :type "const std::array<std::complex<float>, N > &")
						(out :type "std::array<std::complex<float>, N > &"))
					       "template<std::size_t N > void")
			     (setf ,@(loop for i below n appending
					  `((aref out ,(rev i n)) (aref in ,i)))))
		  (function
		   (fft ((in :type "const std::array<std::complex<float>, N > &")
			 (out :type "std::array<std::complex<float>, N > &"))
			"template<std::size_t N > void")
		   (funcall bit_reverse_copy in out)
		   ,@(loop for s from 1 upto (floor (log n 2)) appending
			  (let ((m (expt 2 s)))
			    `((let ((w_m :type "const auto" :init (funcall "std::complex<float>"
									   ,(coerce (cos (/ (* pi -2) m)) 'single-float)
									   ,(coerce (sin (/ (* pi -2) m)) 'single-float))))
				(for ((k 0) (< k N) (+= k ,m))
				     (let ((w :type "std::complex<float>" :ctor 1))
				       (dotimes (j ,(/ m 2))
					 (let ((t :ctor (* w (aref out (+ k j ,(/ m 2)))))
					       (u :ctor (aref out (+ k j)))
					       )
					   (setf (aref out (+ k j)) (+ u t)
						 (aref out (+ k j ,(/ m 2))) (- u t)
						 w (* w w_m)))))))))
			  ))
		  
		  
		  
		  ,@(dox :brief "initialize sdc acquisition"
			 :usage "sets up rx and adc."
			 :params '((uri "address of the iio device")
				   (lo "local oscillator frequency")
				   (fs "adc sample rate"))
			 :return "iio context")
		  (function (pluto_init ((uri :type "const char*")
					 (lo :type uint64_t)
					 (fs :type uint64_t))
					
					"struct iio_context*")
			    (let ((ctx :init (funcall iio_create_context_from_uri uri))
				  (phy :init (funcall iio_context_find_device ctx (string "ad9361-phy")))
				  )
			      (funcall iio_channel_attr_write_longlong
				       (funcall iio_device_find_channel phy (string "altvoltage0") true)
				       (string "frequency")
				       lo)
			      (funcall iio_channel_attr_write_longlong
				       (funcall iio_device_find_channel phy (string "voltage0") true)
				       (string "sampling_frequency")
				       fs)
			      (return ctx)))


		  ,@(dox :brief "read from sdr device"
			 :usage "allocates buffer in first call, reset=true releases that buffer otherwise it will be reused;"
			 :params '()
			 :return "Integer, 0 when success")
		  (function (pluto_read ((ctx :type "struct iio_context*")
					 (reset :type "bool" :default false))
					int)
			    (let (
				  (buf :type "static struct iio_buffer*" :init nullptr)
				  (re :type
				    "static struct iio_channel*"))
			      
			      (if (== nullptr buf)
				  (statements
				   (let ((dev :init (funcall iio_context_find_device ctx (string "cf-ad9361-lpc")))
					 #+nil (re :init (paren-list
						    (let ((ch :init (funcall iio_device_find_channel
									     dev (string "voltage0") 0)))
						      (funcall iio_channel_enable ch)
						      (raw "ch"))))
					 #+nil (im :init (paren-list
						    (let ((ch :init (funcall iio_device_find_channel
									     dev (string "voltage1") 0)))
						      (funcall iio_channel_enable ch)
						      (raw "ch"))))
					 #+nil (buf :init (funcall iio_device_create_buffer dev ,n false)))
				     (setf re (paren-list
					       (let ((ch :init (funcall iio_device_find_channel
									dev (string "voltage0") 0)))
						 (funcall iio_channel_enable ch)
						 (raw "ch")))
					   buf (funcall iio_device_create_buffer dev ,n false))))
				  )
			      (statements ;while true
				   (macroexpand (e "pluto_read refill"))
				   (macroexpand (benchmark (funcall iio_buffer_refill buf)))
				   (macroexpand (e "pluto_read copy"))
				   (macroexpand (benchmark
						  (let ((d :init (funcall iio_buffer_step buf))
							(e :init (funcall iio_buffer_end buf))
							(i :init 0))
						    (for ((p (funcall iio_buffer_first buf re))
							  (< p e)
							  (+= p d)
							  )
							 (let ((pi :init (funcall reinterpret_cast<int16_t*> p))
							       (re :init (aref pi 0))
							       (im :init (aref pi 1)))
							   #+nil (macroexpand (e "i " i))
							   (setf (aref m_fft_in i) (funcall "std::complex<float>"
											     re im))
							   (+= i 1)
							   )
							 ))))
				   (if (&& (!= nullptr buf) reset)
				      (statements (funcall iio_buffer_destroy buf)
						  (setf buf nullptr)))))
			    (return 0))

		  ,@(dox :brief "close sdr device"
			 :usage "destroy iio context"
			 :params '()
			 :return "Integer")
		  (function (pluto_close ((ctx :type "struct iio_context*"))
					 int)
			    (funcall iio_context_destroy ctx)
			    (return 0))
		  
		  
		  
		  ,@(dox :brief "main function"
			 :usage "draw to screen"
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  
		  (function (main ((argc :type int)
				   (argv :type char**))
				  int)

			    (dotimes (i M_MAG_N)
			       (setf (aref m_fft_in i) 0.0
				     (aref m_fft_out i) 0.0
				     (aref m_fft_out_mag i) 0.0))
			    
			    (if (== 0 (funcall drmAvailable))
				(macroexpand (er "drm not available")))

			    
			    (let ((errno :type "extern int")
				  (pluto_ctx :init (funcall pluto_init (string "usb:1.5.5") 2412000000 50000000))
				  (fd :init (paren-list (let ((fd :init (funcall drmOpen (string "i915") nullptr)))
							  (if (< fd 0)
							      (macroexpand (er "drmOpen error: fd=" fd " errno=" errno)))
							  (let ((rr :init (funcall drmSetClientCap fd DRM_CLIENT_CAP_UNIVERSAL_PLANES 1)))
							    (if (!= 0 rr)
								(macroexpand (er "drmSetClientCap error: " rr " errno=" errno))))
							  (raw "fd"))))
				  (res :init  (paren-list (let ((r :init (funcall drmModeGetResources fd)))
							    (funcall assert r)
							    (raw "r"))))
				  (c :type drmModeConnectorPtr
				     :init (paren-list
					    (let ((c :type drmModeConnectorPtr :init nullptr))
					      (dotimes (i (funcall "static_cast<unsigned int>" res->count_connectors))
						(setf c  (funcall drmModeGetConnector fd (aref res->connectors i)))
						(funcall assert c)
						(if (== DRM_MODE_CONNECTED
							c->connection)
						    (break))
						(funcall drmFree c))
					      (raw "c"))))
				  (enc :init (funcall drmModeGetEncoder fd c->encoder_id))
				  (crtc :init (funcall drmModeGetCrtc fd enc->crtc_id))
				  (fb :init (funcall drmModeGetFB fd crtc->buffer_id))
				  (plane_res :init (funcall drmModeGetPlaneResources fd))
				  (plane  :init (paren-list
						 (let ((p :type drmModePlanePtr :init nullptr))
						   (statements
						    ,@(loop for e in '(enc crtc fb plane_res) collect
							   `(funcall assert ,e)))
						   (dotimes (i plane_res->count_planes)
						     (setf p (funcall drmModeGetPlane fd (aref plane_res->planes i)))
						     (funcall assert p)
						     (if (== p->fb_id fb->fb_id)
							 (break))
						     (funcall drmFree p))
						   (raw "p"))))
				  (has_dumb :init (paren-list
						   (let ((has_dumb :type uint64_t :init 0))
						     (funcall assert (! (funcall drmGetCap fd DRM_CAP_DUMB_BUFFER &has_dumb)))
						     (funcall assert has_dumb)
						     (raw "has_dumb"))))
				  (creq :init
				    (paren-list
				     (let ((creq :type "struct drm_mode_create_dumb"))
				       (funcall memset &creq 0 (funcall sizeof creq))
				       (setf creq.width fb->width
					     creq.height fb->height
					     creq.bpp fb->bpp)
				       (funcall assert (! (funcall drmIoctl fd DRM_IOCTL_MODE_CREATE_DUMB &creq)))
				       (macroexpand (e "width=" creq.width " height=" creq.height))
				       (raw "creq"))))
				  (my_fb :init (paren-list
						(let ((my_fb :type uint32_t :init 0))
						  (funcall assert
							   (! (funcall drmModeAddFB fd creq.width creq.height 24
								       creq.bpp creq.pitch creq.handle &my_fb)))
						  (raw
						   "my_fb"))))
				  (flip :init (paren-list
					       (let ((gp :type "struct drm_i915_getparam"
							 )
						     (value :type int))
						 (funcall memset &gp 0 (funcall sizeof gp))
						 (setf gp.param I915_PARAM_HAS_PAGEFLIPPING
						       gp.value &value)
						 (let ((r :init (funcall drmCommandWriteRead fd DRM_I915_GETPARAM
									 &gp (funcall sizeof gp))))
						   (if r
						       (macroexpand (er

								     "i915_getparam " r))))
						 (macroexpand (e
							       "flipping=" *gp.value))
						 (raw "*gp.value"))))
				  (mreq :init (paren-list
					       (let ((mreq :type "struct drm_mode_map_dumb"))
						 (funcall memset &mreq 0 (funcall sizeof mreq))
						 (setf mreq.handle creq.handle)
						 (funcall assert
							  (! (funcall drmIoctl fd DRM_IOCTL_MODE_MAP_DUMB &mreq)))
						 (raw "mreq"))))
				  (map :init (paren-list
					      (let ((map :type uint32_t*
							 :init (funcall static_cast<uint32_t*>
									(funcall mmap 0 creq.size
										 (\| PROT_READ PROT_WRITE)
										 MAP_SHARED fd mreq.offset))))
						(funcall assert (!= MAP_FAILED map))
						(funcall memset map 0 creq.size)
						(raw "map")))))
			      
			      
			      (dotimes (i 256)
				(dotimes (j 256)
				  (setf (aref map (+ j (* i (>> creq.pitch 2))))
					(hex #x12345678))))
			      (funcall assert (! (funcall drmModeSetCrtc fd crtc->crtc_id my_fb
							  0 0 &c->connector_id 1 &crtc->mode)))
			      (dotimes (k 1)
				(dotimes (i 120 #+nil creq.height)
				  (macroexpand (e
						 "pluto_read"))
				  (macroexpand (benchmark
						 (funcall pluto_read pluto_ctx)))
				  (macroexpand (e
						 "fft"))
				  (macroexpand
				   (benchmark
				     
				     (funcall fft m_fft_in m_fft_out)
				     ))
				  (macroexpand (e
						 "scale"))
				  (macroexpand
				   (benchmark
				     
				     
				     (dotimes  (i M_MAG_N)
				       (setf (aref m_fft_out_mag i) (* 10 (funcall
									   "std::log10"
									   (funcall "std::abs" (aref m_fft_out i))))))
				     ))
				  (macroexpand (e
						 "out"))
				  (macroexpand
				   (benchmark
				     
				     
				     
				     (let ((mi :init 50s0 #+nil (paren-list
								 (let ((mi :init (aref m_fft_out_mag 0)))
								   (dotimes (i M_MAG_N)
								     (let ((val :init (aref m_fft_out_mag i)))
								       (if (< val mi)
									   (setf mi val))))
								   (raw "mi"))))
					   (ma :init 0s0 #+nil (paren-list
								(let ((ma :init (aref m_fft_out_mag 0)))
								  (dotimes (i M_MAG_N)
								    (let ((val :init (aref m_fft_out_mag i)))
								      (if (< ma val)
									  (setf ma val))))
								  (raw "ma"))))
					   (s :init (/ 255s0 (- ma mi))))
				       #+nil (macroexpand (e "ma " ma "mi " mi))
				       (dotimes (j (funcall "std::min" ,n (funcall static_cast<int> creq.width)))
					 (setf (aref map (+ j (* i (>> creq.pitch 2))))
					       (* s (- (aref m_fft_out_mag j) mi))
					       #+nil (+ k (hex #x12345678))))))))
				#+nil (funcall usleep 32000))

			      #+nil (funcall sleep 1)
			      (funcall assert (! (funcall drmModeSetCrtc fd crtc->crtc_id fb->fb_id
							  0 0 &c->connector_id 1 &crtc->mode)))
			      (funcall assert (! (funcall drmModeRmFB fd my_fb)))
			      (let ((dreq :type "struct drm_mode_destroy_dumb"))
				(funcall memset &dreq 0 (funcall sizeof dreq))
				(setf dreq.handle creq.handle)
				(funcall assert (! (funcall drmIoctl fd DRM_IOCTL_MODE_DESTROY_DUMB &dreq))))
			      (statements
			       ,@(loop for e in '(plane plane_res fb crtc enc c res) collect
				      `(funcall drmFree ,e)))
			      (funcall drmClose fd)
			      (funcall pluto_close pluto_ctx))
			    
			    
			    
			    
			    (return 0)))))
    (write-source "stage/cl-gen-drm-waterfall/source/main" "cpp" code)))


