protocol
removeLast

	| result |
	result := value removeLast.
	self valueRemoved: result.
	^ result