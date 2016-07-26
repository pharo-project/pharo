visiting
visitArrayNode: anArrayNode
	self bracketWith: '{}' around: [ self formatArray: anArrayNode ]