protocol
removeAll: aCollection
	| result |
	result := value removeAll: aCollection.
	self valueRemoved: result.
	^ result