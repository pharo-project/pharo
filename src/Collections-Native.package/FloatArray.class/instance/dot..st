arithmetic
dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."
       | result |
	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>
	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i))].
	^result