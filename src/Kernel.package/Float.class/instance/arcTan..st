mathematical functions
arcTan: denominator
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Implementation note: use sign in order to catch cases of negativeZero"

	^self = 0.0
		ifTrue: [denominator sign >= 0
			ifTrue: [ 0 ]
			ifFalse: [ self sign >= 0
				ifTrue: [ Pi ]
				ifFalse: [ Pi negated ]]]
		ifFalse: [denominator = 0.0
			ifTrue: [self > 0.0
				ifTrue: [ Halfpi ]
				ifFalse: [ Halfpi negated ]]
			ifFalse: [denominator > 0
				ifTrue: [ (self / denominator) arcTan ]
				ifFalse: [self > 0
					ifTrue: [ ((self / denominator) arcTan) + Pi ]
					ifFalse: [ ((self / denominator) arcTan) - Pi ]]]]