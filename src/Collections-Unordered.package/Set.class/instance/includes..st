testing
includes: anObject 
	^ (array at: (self findElementOrNil: anObject)) ~~ nil