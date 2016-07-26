protocol
atAll: aCollection put: anObject 
	
	| result |
	result := value atAll: aCollection put: anObject .
	self valueAdded: anObject.
	^ result