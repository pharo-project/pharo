protocol
removeFirst: n 

	| result |
	result := value removeFirst: n .
	self valueRemoved: result.
	^ result