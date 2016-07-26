testing
isMovingToArgument
	^(parseTree arguments collect: [:each | each name]) includes: variable