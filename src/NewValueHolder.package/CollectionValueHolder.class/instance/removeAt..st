protocol
removeAt: index

	| result |
	result := value removeAt: index.
	self valueRemoved: result.
	^ result