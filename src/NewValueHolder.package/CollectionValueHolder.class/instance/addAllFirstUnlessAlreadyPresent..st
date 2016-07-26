protocol
addAllFirstUnlessAlreadyPresent: anOrderedCollection
	
	| result |
	result := value addAllFirstUnlessAlreadyPresent: anOrderedCollection.
	self valueAdded: anOrderedCollection.
	^ result