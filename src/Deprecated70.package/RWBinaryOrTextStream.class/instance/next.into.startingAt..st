accessing
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than n elements have been read."
	"Overriden for efficiency"
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]