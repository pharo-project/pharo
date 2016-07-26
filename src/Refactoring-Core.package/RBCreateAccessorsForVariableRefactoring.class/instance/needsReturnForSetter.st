testing
needsReturnForSetter
	needsReturn isNil 
		ifTrue: 
			[needsReturn := self 
						usesAssignmentOf: variableName
						in: class
						classVariable: classVariable].
	^needsReturn