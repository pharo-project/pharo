private-accessing
getClassesToMoveTo
	self isMovingToArgument 
		ifTrue: [self getClassesForTemporaryVariable]
		ifFalse: 
			[self isMovingToInstVar 
				ifTrue: [self getClassesForInstanceVariable]
				ifFalse: [self getClassForGlobalOrClassVariable]].
	moveToClasses isEmpty 
		ifTrue: [self refactoringFailure: 'No classes selected, method not moved.']