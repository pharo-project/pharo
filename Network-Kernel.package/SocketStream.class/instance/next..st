stream in
next: anInteger
	"Read count elements and return them in a collection.
	If the receiver is #atEnd before count elements were read, 
	return a smaller collection and don't signal ConnectionClosed."

	^ self nextInto: (self streamBuffer: anInteger)