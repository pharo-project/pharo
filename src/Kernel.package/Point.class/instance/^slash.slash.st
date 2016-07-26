arithmetic
// arg 
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint ifTrue: [^ (x // arg x) @ (y // arg y)].
	^ arg adaptToPoint: self andSend: #//