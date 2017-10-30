evaluating
threeWayCompare: anObject1 with: anObject2
	"If the result of current function is 0, then pass on to the next function to work it out"

	| result |
	result := startFunction collate: anObject1 with: anObject2.
	^result isZero
		ifTrue: [nextFunction collate: anObject1 with: anObject2.]
		ifFalse: [result]