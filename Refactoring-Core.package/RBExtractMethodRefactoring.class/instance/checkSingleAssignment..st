transforming
checkSingleAssignment: varName 
	((RBReadBeforeWrittenTester isVariable: varName
		readBeforeWrittenIn: extractedParseTree) 
			or: [extractedParseTree containsReturn]) 
			ifTrue: 
				[self 
					refactoringError: 'Cannot extract assignments to temporaries without all references'].
	extractedParseTree 
		addNode: (RBReturnNode value: (RBVariableNode named: varName)).
	modifiedParseTree := RBParseTreeRewriter 
				replace: self methodDelimiter
				with: varName , ' := ' , self methodDelimiter
				in: modifiedParseTree