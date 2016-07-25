preconditions
verifySelectedInterval
	sourceTree := class parseTreeFor: selector.
	sourceTree isNil ifTrue: [self refactoringFailure: 'Could not parse source'].
	assignmentNode := sourceTree whichNodeIsContainedBy: sourceInterval.
	assignmentNode isAssignment 
		ifFalse: 
			[self refactoringFailure: 'The selected node is not an assignment statement'].
	definingNode := assignmentNode whoDefines: assignmentNode variable name.
	self hasOnlyOneAssignment 
		ifFalse: 
			[self refactoringError: 'There are multiple assignments to the variable'].
	(RBReadBeforeWrittenTester isVariable: assignmentNode variable name
		writtenBeforeReadIn: definingNode) 
			ifFalse: 
				[self 
					refactoringError: 'The variable is possible read before it is assigned']