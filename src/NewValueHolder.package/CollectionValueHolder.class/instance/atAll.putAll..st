protocol
atAll: indexArray putAll: valueArray
	
	| result |
	result := value atAll: indexArray putAll: valueArray.
	self valueAdded: valueArray.
	^ result