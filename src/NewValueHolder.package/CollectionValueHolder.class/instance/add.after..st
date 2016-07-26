protocol
add: newObject after: oldObject 
	
	| result |
	result := value add: newObject after: oldObject.
	self valueAdded: newObject.
	^ result