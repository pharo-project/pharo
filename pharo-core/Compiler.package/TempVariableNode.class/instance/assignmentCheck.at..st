testing
assignmentCheck: encoder at: location
	^(self isBlockArg or: [self isMethodArg])
			ifTrue: [location]
			ifFalse: [-1]