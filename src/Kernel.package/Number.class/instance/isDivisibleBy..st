testing
isDivisibleBy: aNumber
	aNumber = 0 ifTrue: [^ false].
	aNumber isInteger ifFalse: [^ false].
	^ (self \\ aNumber) = 0