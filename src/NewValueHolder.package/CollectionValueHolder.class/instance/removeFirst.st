protocol
removeFirst

	| result |
	result := value removeFirst.
	self valueRemoved: result.
	^ result