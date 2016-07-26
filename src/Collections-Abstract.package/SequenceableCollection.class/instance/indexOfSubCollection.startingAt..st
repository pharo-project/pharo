accessing
indexOfSubCollection: aSubCollection startingAt: anIndex 
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal 
	the rest of the elements of aSubCollection. Begin the search at element 
	anIndex of the receiver. If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]