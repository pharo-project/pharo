protocol
add: newObject
	| result |
	result := value add: newObject.
	self valueAdded: newObject.
	^ result