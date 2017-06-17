testing
hasNext
	^ hasNext ifNil: [
		self basicNext.
		hasNext ]