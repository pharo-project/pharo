accessing
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset.
	Return the actual number of elements read."

	| read toRead |
	read := 0.
	[ read < requestedCount and: [ self atEnd not] ] whileTrue: [
		toRead := (limit - position) min: (requestedCount - read).
		collection 
			replaceFrom: offset + read 
			to: offset + read + toRead - 1 
			with: chunk 
			startingAt: position + 1.
		read := read + toRead.
		position := position + toRead ].
	^ read