accessing
indexOfAnyOf: aCollection startingAt: start ifAbsent: aBlock
	"Use double dispatching for speed"
	| index |
	^(index := aCollection findFirstInByteString: self startingAt: start) = 0
		ifTrue: [aBlock value]
		ifFalse: [index]