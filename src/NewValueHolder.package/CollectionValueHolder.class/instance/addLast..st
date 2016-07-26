protocol
addLast: newObject 

	| result |
	result := value addLast: newObject .
	self valueAdded: newObject.
	^ result