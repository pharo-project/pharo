protocol
add: newObject before: oldObject
	
	| result |
	result := value add: newObject before: oldObject.
	self valueAdded: newObject.
	^ result