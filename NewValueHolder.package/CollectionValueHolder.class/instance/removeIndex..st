protocol
removeIndex: removedIndex

	| result |
	result := value removeIndex: removedIndex.
	self valueRemoved: result.
	^ result