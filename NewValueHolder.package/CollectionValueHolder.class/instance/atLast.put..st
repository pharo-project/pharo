protocol
atLast: indexFromEnd put: obj
	
	| result |
	result := value atLast: indexFromEnd put: obj.
	self valueAdded: obj.
	^ result