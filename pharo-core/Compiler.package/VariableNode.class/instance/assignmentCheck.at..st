testing
assignmentCheck: encoder at: location
	^(encoder cantStoreInto: name) ifTrue: [location] ifFalse: [-1]