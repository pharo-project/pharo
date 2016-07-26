protocol
addAll: aCollection 
	
	| result |
	result := value addAll: aCollection.
	self valueAdded: aCollection.
	^ result