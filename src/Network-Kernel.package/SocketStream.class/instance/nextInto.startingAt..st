stream in
nextInto: aCollection startingAt: startIndex
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.