mathematical functions
timesTwoPower: anInteger 
	"Primitive. Answer with the receiver multiplied by 2.0 raised
	to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 554>

	anInteger < -29 ifTrue: [^ self * (2.0 raisedToInteger: anInteger)].
	anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
	anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat].
	^ self * (2.0 raisedToInteger: anInteger)