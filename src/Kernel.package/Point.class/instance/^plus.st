arithmetic
+ arg 
	"Answer a Point that is the sum of the receiver and arg."

	arg isPoint ifTrue: [^ (x + arg x) @ (y + arg y)].
	^ arg adaptToPoint: self andSend: #+