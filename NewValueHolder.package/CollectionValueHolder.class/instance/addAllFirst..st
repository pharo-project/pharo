protocol
addAllFirst: anOrderedCollection
	
	| result |
	result := value addAllFirst: anOrderedCollection.
	self valueAdded: anOrderedCollection.
	^ result