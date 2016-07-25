testing
rangeIncludes: aNumber
	"Return true if the number lies in the interval between start and stop."

	step >= 0
		ifTrue: [^ aNumber between: start and: stop]
		ifFalse: [^ aNumber between: stop and: start]
