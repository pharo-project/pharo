*Collections-arithmetic
/= anObject
	^anObject isNumber
		ifTrue:[self primDivScalar: anObject asFloat]
		ifFalse:[self primDivArray: anObject]