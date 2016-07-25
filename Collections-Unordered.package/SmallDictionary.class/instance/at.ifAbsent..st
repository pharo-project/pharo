accessing - values
at: key ifAbsent: aBlock 
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| index |
	index := self findIndexForKey:  key.
	index = 0 ifTrue: [^ aBlock value].
	
	^ values at: index.

	"| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [^ aBlock value].
	^ assoc value"