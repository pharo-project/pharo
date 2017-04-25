evaluating
value: anObject value: bObject
	"Refinement of the parent behavior. If the result of my collator is 0, then pass on to the next variable to work it out."

	| result |
	result := (collator value: anObject value: bObject) * direction.
	^result isZero
		ifTrue: [next value: anObject value: bObject]
		ifFalse: [result < 0]