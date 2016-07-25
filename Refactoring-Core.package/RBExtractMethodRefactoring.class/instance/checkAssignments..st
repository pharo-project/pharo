transforming
checkAssignments: variableNames 
	| node outsideVars removeAssigned |
	removeAssigned := variableNames copy.
	node := self placeholderNode.
	outsideVars := variableNames 
				select: [:each | (node whoDefines: each) references: each].
	outsideVars size == 1 
		ifTrue: [self checkSingleAssignment: outsideVars asArray first].
	outsideVars size > 1 
		ifTrue: 
			[self refactoringError: 'Cannot extract assignment without all references.'].
	removeAssigned removeAll: outsideVars.
	(RBReadBeforeWrittenTester readBeforeWritten: removeAssigned
		in: extractedParseTree) isEmpty 
		ifFalse: 
			[self refactoringError: 'Cannot extract assignment if read before written.'].
	removeAssigned 
		do: [:each | (node whoDefines: each) removeTemporaryNamed: each].
	self createTemporariesInExtractedMethodFor: variableNames