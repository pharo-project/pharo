accessing
upToAnyOf: aCollection do: aBlock
	"Overriden for speed"
	| end result |
	end := collection indexOfAnyOf: aCollection startingAt: 1 + position ifAbsent: [0].
	(end = 0 or: [end > readLimit]) ifTrue: [^self upToEnd].
	result := collection copyFrom: 1 + position to: -1 + end.
	position := end.
	aBlock value: (collection at: end).
	^result