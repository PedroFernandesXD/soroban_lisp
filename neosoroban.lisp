(defun digits(n &optional (r nil))
  (if (< n 10)
      (cons n r) 
      (multiple-value-bind (a b) (floor n 10)
	(digits a (cons b r)) )))

(defun mksoroban(x y n &optional
			 (xs (digits x))
			 (ys (digits y))
			 (xsz (length xs))
			 (ysz (length ys))
	    (v (make-array n :element-type '(mod 64)) ))
  (loop for d in xs
        for i from 0 to 100 do
       (setf (aref v i) d))
  (loop 
     for d in ys
     for i from (- n xsz ysz 1) to 100 do
       (setf (aref v i) d))
  (values #|jx=|# (- xsz  1)
		  #|iy=|# (- n xsz ysz 1)
		  #|jy=|# (- n xsz ysz)
		  #|jr=|# (+  (- n xsz ysz) 1) v))

(defun normalize(soro n jr)
  (loop for i from (- n 1) downto (+ jr 1)
       for carry = (floor (aref soro i) 10)
       then (floor (aref soro i) 10)
       do (setf (aref soro i) (mod (aref soro i) 10)
		(aref soro (- i 1))
		(+ (aref soro (- i 1)) carry)) ))


(defun cpy(v)
  (loop with w = (make-array (length v) :initial-element 0)
     for i from 0 to 100
     for x across v do (setf (aref w i) x)
       finally (return w)))


(defun mult(x y &optional (n 11) (r nil))
  (multiple-value-bind (jx iy jy jr soro)
	(mksoroban x y n)
      (push (cpy soro) r)
      (loop for k from jy downto iy do
	   (setf (aref soro (+ k 1)) 0)
	   (loop with m = (aref soro k)
	      for i from 0 to jx
	      for j from jr to (- n 2) do
		(let* ((xss (digits (* (aref soro i) m)))
		       (x1 (if (cdr xss) (car xss) 0))
		       (x2 (if (cdr xss) (cadr xss) (car xss))))
		  (setf (aref soro j) (+ (aref soro j) x1)
			(aref soro (+ j 1)) (+ (aref soro (+ j 1)) x2)))
		(normalize soro n jr)
		(push (cpy soro) r))
	   (setf (aref soro k) 0)
	   (decf jr))
      (push (cpy soro) r))
  (reverse r))

(defun soro-simples(xs &optional (s t))
  (format s "~%~a~%~{ \\tige{~a}{~a}{~a}~%~}\\cadre{~a}~%~a~%"
    "\\begin{tikzpicture}"
    (loop for i from 1 to 100 for x across xs
	     append (list i x (if (= (mod i 3) 0) 1 0)) )
    (length xs)
    "\\end{tikzpicture}"))

(defun seq-aux(n &optional (r nil))
  (if (zerop n) r
      (loop for x from 5 downto (- 5 (- n 1)) do
	   (push x r)
	 finally (return r)) ))

(defun seq(n)
  (if (< n 5)
      (seq-aux n)
      (cons 10 (seq-aux (- n 5))) ))


(defun soro-color(xs &optional (s t) (w 0.4) (c "gray"))
  (format s "~%\\begin{minipage}{~a\\textwidth}~%~a~%"
	  w
	  "\\begin{tikzpicture}")
  (loop for i from 1 to 100 for x across xs do
	(format s
		" \\tige{~a}{~a}{~a}~%" i x
		(if (zerop (mod i 3)) 1 0))
       (unless (zerop x) ;; ADICIONAR OUTRA CONDIÇÃO VECTOR
	  (format s
		  "~{ \\binoire{~a}{~a}{~a}~%~}"
		  (loop for j in (seq x) append (list i j c))) ))
  (format s "\\cadre{~a}~%\\end{tikzpicture}~%\\end{minipage}~%"
	  (length xs)) )
