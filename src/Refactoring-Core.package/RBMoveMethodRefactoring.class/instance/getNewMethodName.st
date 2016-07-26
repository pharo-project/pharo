preconditions
getNewMethodName
	| newSelector parameters alreadyDefined methodName newMethodName |
	self removeArgument.
	parameters := parseTree argumentNames asOrderedCollection.
	"parameters remove: variable ifAbsent: []."
	self needsToReplaceSelfReferences 
		ifTrue: [parameters add: selfVariableName].
	methodName := RBMethodName selector: (self uniqueMethodNameFor: parameters size) arguments: parameters.
	
	[newMethodName := self requestMethodNameFor: methodName.
	newMethodName isNil ifTrue: [self refactoringFailure: 'Did not move method'].
	newMethodName isValid 
		ifTrue: [newSelector := newMethodName selector]
		ifFalse: [self refactoringWarning: 'Invalid method name'].
	parameters := newMethodName arguments.
	(self checkMethodName: newSelector in: class) 
		ifFalse: 
			[self refactoringWarning: newSelector , ' is not a valid selector name.'.
			newSelector := nil].
	alreadyDefined := moveToClasses 
				detect: [:each | each hierarchyDefinesMethod: newSelector]
				ifNone: [nil].
	alreadyDefined notNil 
		ifTrue: 
			[self 
				refactoringWarning: ('<1s> is already defined by <2p> or a super/subclass<n>Try another?' 
						expandMacrosWith: newSelector
						with: alreadyDefined).
			newSelector := nil].
	newSelector isNil] 
			whileTrue: [].
	parseTree
		renameSelector: newSelector
		andArguments: (parameters collect: [:each | RBVariableNode named: each]) 
			asArray