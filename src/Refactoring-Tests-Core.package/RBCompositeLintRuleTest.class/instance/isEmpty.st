testing
isEmpty
	^(rules detect: [:each | each isEmpty not] ifNone: [nil]) isNil