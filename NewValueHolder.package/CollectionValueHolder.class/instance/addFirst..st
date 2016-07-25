protocol
addFirst: newObject 

	| result |
	result := value addFirst: newObject .
	self valueAdded: newObject.
	^ result