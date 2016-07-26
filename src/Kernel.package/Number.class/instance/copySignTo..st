mathematical functions
copySignTo: aNumber
	"Return a number with same magnitude as aNumber and same sign as self."

	^ self positive
		ifTrue: [aNumber abs]
		ifFalse: [aNumber withNegativeSign].