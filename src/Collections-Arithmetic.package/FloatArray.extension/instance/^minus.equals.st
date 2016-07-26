*Collections-arithmetic
-= anObject
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]