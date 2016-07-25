protocol
addAllLast: aCollection 
	
	| result |
	result := value addAllLast: aCollection.
	self valueAdded: aCollection.
	^ result