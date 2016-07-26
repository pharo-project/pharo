protocol
atAllPut: anObject 
	
	| result |
	result := value atAllPut: anObject.
	self valueAdded: anObject.
	^ result