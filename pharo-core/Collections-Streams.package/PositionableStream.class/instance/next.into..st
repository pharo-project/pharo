accessing
next: n into: aCollection
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1