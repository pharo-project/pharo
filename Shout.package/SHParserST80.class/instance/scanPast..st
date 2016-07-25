scan
scanPast: rangeType 
	rangeType = #blockPatternArg
		ifTrue: [self pushArgument: currentToken].
	rangeType = #blockPatternTempVar
		ifTrue: [self pushTemporary: currentToken].
	rangeType = #patternArg
		ifTrue: [self pushArgument: currentToken].
	rangeType = #patternTempVar
		ifTrue: [self pushTemporary: currentToken].
	^ self rangeType: rangeType;
		 scanNext