accessing
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	"Overriden for efficiency"
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + max - 1
		with: collection
		startingAt: position + 1.
	position := position + max.
	^ max