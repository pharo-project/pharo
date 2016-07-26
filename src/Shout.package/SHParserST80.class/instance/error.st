error handling
error
	self
		rangeType: #excessCode
		start: (ranges isEmpty
				ifTrue: [1]
				ifFalse: [ranges last end + 1])
		end: source size.
	errorBlock value