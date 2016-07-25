reading
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available."
	
	| toRead |
	toRead := requestedCount min: self availableForReading.
	collection 
		replaceFrom: offset 
		to: offset + toRead - 1 
		with: buffer 
		startingAt: self contentsStart.
	self advanceReadPointer: toRead.
	^ toRead