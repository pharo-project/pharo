accessing
at: index put: value
	"Store the given 16-bit integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [ 
			(index between: 1 and: self size)
				ifTrue: [ self errorImproperStore ]
				ifFalse: [ self errorSubscriptBounds: index ] ].
	index isNumber ifTrue: [ ^ self at: index truncated put: value ].
	self errorNonIntegerIndex