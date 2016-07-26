reading
readInto: collection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."

	position := position + n.
	^ n