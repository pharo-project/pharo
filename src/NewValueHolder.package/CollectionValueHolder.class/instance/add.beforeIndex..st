protocol
add: newObject beforeIndex: index 
	
	| result |
	result := value add: newObject beforeIndex: index.
	self valueAdded: newObject.
	^ result