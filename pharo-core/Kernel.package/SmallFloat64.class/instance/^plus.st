arithmetic
+ aNumber 
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 541>
	^ aNumber adaptToFloat: self andSend: #+