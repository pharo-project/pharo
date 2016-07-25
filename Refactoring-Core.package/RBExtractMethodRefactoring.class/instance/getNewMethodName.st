transforming
getNewMethodName
	| newSelector methodName newMethodName |
	methodName := RBMethodName new.
	methodName arguments: parameters.
	
	[newMethodName := self requestMethodNameFor: methodName.
	newMethodName isNil 
		ifTrue: [self refactoringFailure: 'Did not extract code'].
	newSelector := newMethodName selector.
	(self checkMethodName: newSelector in: class) 
		ifFalse: 
			[self refactoringWarning: newSelector , ' is not a valid selector name.'.
			newSelector := nil].
	(class hierarchyDefinesMethod: newSelector asSymbol) 
		ifTrue: 
			[(self shouldOverride: newSelector in: class) ifFalse: [newSelector := nil]].
	newSelector isNil] 
			whileTrue: [].
	parameters := newMethodName arguments asOrderedCollection.
	^newSelector asSymbol