Extension { #name : #Float }

{ #category : #'*NewMethodFinder' }
Float class >> approvedSelectors [

	 ^ #(asApproximateFraction asApproximateFractionAtOrder: asFraction asTrueFraction
	     reciprocalFloorLog: reciprocalLogBase2 significand significanAsInteger hex
	     isFinite isPowerOfTwo pointerDOuble pointerFloat predecessor literalEqual: safeArcCos successor ulp
		  timesTwoPower: exponent "in the subclasses"
		 )
]
