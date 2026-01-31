(deftemplate carga-actual
	0 100 %
	(
	(vacio(20 1)(30 0))
	(medio(20 0)(40 1)(50 1)(60 0))
	(recomendado(50 0)(60 1)(70 1)(90 0))
	(completo(80 0)(90 1))
	)
)

(deftemplate proximo-viaje
	0 400 km
	(
	(breve(100 1)(150 0))
	(estandar(100 0)(150 1)(200 1)(300 0))
	(largo(200 0)(300 1))
	)
)

(deftemplate tipo-carga
	2 20 kW
	(
	(lenta(2 0)(4 1)(8 0))
	(media(6 0)(10 1)(14 1)(16 0))
	(rapida(14 0)(16 1))
	)
)

(deftemplate tiempo-carga
	10 480 min 
	(
	(corta-duracion(10 0)(40 1)(60 0)) 
      	(media-duracion(40 0)(90 1)(120 1)(180 0)) 
    	(larga-duracion(120 0)(240 1))
	)
) 

(deftemplate bateria
	(slot carga-actual-crisp (type FLOAT))
	(slot proximo-viaje-crisp (type FLOAT))
	(slot mom-tipo-carga-crisp (type FLOAT))
	(slot max-tipo-carga-crisp (type FLOAT))
	(slot mom-tiempo-carga-crisp (type FLOAT))
	(slot max-tiempo-carga-crisp (type FLOAT))
	(slot super-carga (type SYMBOL))
)


(deffacts por-defecto
	(bateria 
	(carga-actual-crisp 0.0) 
	(proximo-viaje-crisp 0.0)
	(mom-tipo-carga-crisp 0.0)
	(max-tipo-carga-crisp 0.0)
	(mom-tiempo-carga-crisp 0.0)
	(max-tiempo-carga-crisp 0.0)
	(super-carga OFF)
	)
)

;FUZZIFY
(deffunction fuzzify (?fztemplate ?value ?delta)

        (bind ?low (get-u-from ?fztemplate))
        (bind ?hi  (get-u-to   ?fztemplate))

        (if (<= ?value ?low)
          then
            (assert-string
              (format nil "(%s (%g 1.0) (%g 0.0))" ?fztemplate ?low ?delta))
          else
            (if (>= ?value ?hi)
              then
                (assert-string
                   (format nil "(%s (%g 0.0) (%g 1.0))"
                               ?fztemplate (- ?hi ?delta) ?hi))
              else
                (assert-string
                   (format nil "(%s (%g 0.0) (%g 1.0) (%g 0.0))"
                               ?fztemplate (max ?low (- ?value ?delta))
                               ?value (min ?hi (+ ?value ?delta)) ))
            )
        )
  )



;LECTURA INPUT
(defrule leer-inputs
	?i <- (initial-fact)
	?bat <- (bateria)
=>
	(retract ?i)
	(printout t "Introduzca la carga actual (%):" crlf)
	(bind ?Rcarga-actual-crisp (read))
	(printout t "Introduzca la distancia del proximo viaje (Km):" crlf)
	(bind ?Rproximo-viaje-crisp (read))
	(printout t "Introduzca ON u OFF en funcion de si quiere super-carga:" crlf)
	(bind ?Rvalor-super-carga OFF)

	(modify ?bat
	(carga-actual-crisp ?Rcarga-actual-crisp)
	(proximo-viaje-crisp ?Rproximo-viaje-crisp)
	(super-carga ?Rvalor-super-carga)
	)
	
	;Fusificacion
	(fuzzify carga-actual ?Rcarga-actual-crisp 0)
	(fuzzify proximo-viaje ?Rproximo-viaje-crisp 10)
)



;REGLAS
;;;;;;;;;;;;;; TIPO CARGA Y TIEMPO CARGA
(defrule super-carga-on
	(declare (salience 100))
	?bat <- (bateria (super-carga ON))
=>
	(assert(tipo-carga very rapida))
)

(defrule vacio-breve
	(carga-actual vacio)
	(proximo-viaje breve)
=>
	(assert(tipo-carga media))
	(assert(tiempo-carga extremely media-duracion))
)

(defrule medio-breve
	(carga-actual medio)
	(proximo-viaje breve)
=>
	(assert(tipo-carga lenta))
	(assert(tiempo-carga somewhat larga-duracion))
)

(defrule recomendado-breve
	(carga-actual recomendado)
	(proximo-viaje breve)
=>
	(assert(tipo-carga very lenta))
	(assert(tiempo-carga media-duracion))
)

(defrule completo-breve
	(carga-actual completo)
	(proximo-viaje breve)
=>
	(assert(tipo-carga extremely lenta))
	(assert(tiempo-carga extremely corta-duracion))
)
(defrule vacio-estandar
	(carga-actual vacio)
	(proximo-viaje estandar)
=>
	(assert(tipo-carga more-or-less rapida))
	(assert(tiempo-carga larga-duracion))
)

(defrule medio-estandar
	(carga-actual medio)
	(proximo-viaje estandar)
=>
	(assert(tipo-carga somewhat media))
	(assert(tiempo-carga media-duracion))
)

(defrule recomendado-estandar
	(carga-actual recomendado)
	(proximo-viaje estandar)
=>
	(assert(tipo-carga very lenta))
	(assert(tiempo-carga media-duracion))
)

(defrule completo-estandar
	(carga-actual completo)
	(proximo-viaje estandar)
=>
	(assert(tipo-carga extremely lenta))
	(assert(tiempo-carga very corta-duracion))
)
(defrule vacio-largo
	(carga-actual vacio)
	(proximo-viaje largo)
=>
	(assert(tipo-carga very rapida))
	(assert(tiempo-carga media-duracion))
)

(defrule medio-largo
	(carga-actual medio)
	(proximo-viaje largo)
=>
	(assert(tipo-carga media))
	(assert(tiempo-carga more-or-less larga-duracion))
)

(defrule recomendado-largo
	(carga-actual recomendado)
	(proximo-viaje largo)
=>
	(assert(tipo-carga lenta))
	(assert(tiempo-carga corta-duracion))
)

(defrule completo-largo
	(carga-actual completo)
	(proximo-viaje largo)
=>
	(assert(tipo-carga very lenta))
	(assert(tiempo-carga corta-duracion))
)

;DEFUSIFICAR
(defrule defusificar
	(declare (salience -999))
	?bat <- (bateria)
	(tipo-carga ?tipo)
    	(tiempo-carga ?tiempo)
 =>

	(bind ?e1 (moment-defuzzify ?tipo))
	(bind ?e2 (maximum-defuzzify ?tipo))
	(bind ?e3 (moment-defuzzify ?tiempo))
	(bind ?e4 (maximum-defuzzify ?tiempo))

	(modify ?bat
	(mom-tipo-carga-crisp ?e1)
	(max-tipo-carga-crisp ?e2)
	(mom-tiempo-carga-crisp ?e3)
	(max-tiempo-carga-crisp ?e4)
	)
	
	(printout t crlf "Resultados defusificados:" crlf)
	(printout t "mom-tipo-carga-crisp: " ?e1 crlf)
	(printout t "max-tipo-carga-crisp: " ?e2 crlf)
	(printout t "mom-tiempo-carga-crisp: " ?e3 crlf)
	(printout t "max-tiempo-carga-crisp: " ?e4 crlf)
	(halt)
)
