private
pvtAt: index put: value
	"Private -- for swapping only"

	<primitive: 144>
	index isInteger
		ifTrue: [ 
			(index between: 1 and: self size)
				ifTrue: [ self errorImproperStore ]
				ifFalse: [ self errorSubscriptBounds: index ] ].
	index isNumber ifTrue: [ ^ self at: index truncated put: value ].
	self errorNonIntegerIndex