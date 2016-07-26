private
isStartOfMethodDefinition
	^ completionIndex = 1 or: [ self isPositionAtPatternArg ]