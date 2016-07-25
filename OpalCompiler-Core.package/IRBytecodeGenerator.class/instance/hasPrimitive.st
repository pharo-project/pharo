results
hasPrimitive
	"do I have a primitive? Both normal primitive and quick return"
	primNumber > 0 ifTrue: [ ^true ].
	^ self endPrimNumber > 0