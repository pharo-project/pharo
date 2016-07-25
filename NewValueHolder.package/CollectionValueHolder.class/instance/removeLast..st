protocol
removeLast: n

	| result |
	result := value removeLast: n.
	self valueRemoved: result.
	^ result