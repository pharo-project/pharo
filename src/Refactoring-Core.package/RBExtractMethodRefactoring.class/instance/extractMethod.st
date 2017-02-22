transforming
extractMethod
	| parseTree isSequence extractCode subtree newCode errorMessage |
	extractCode := self getExtractedSource.
	extractedParseTree := RBParser parseExpression: extractCode
				onError: [:string :pos :parser |  errorMessage := string. parser parseErrorNode: string ].
	extractedParseTree isNil 
		ifTrue: [self refactoringFailure: 'Invalid source to extract'].
		
	extractedParseTree isFaulty
		ifTrue: [self refactoringFailure: 'Invalid source to extract - ', errorMessage.].
		
	(extractedParseTree isSequence 
		and: [extractedParseTree statements isEmpty]) 
			ifTrue: [self refactoringError: 'Select some code to extract'].
	isSequence := extractedParseTree isSequence 
				or: [extractedParseTree isReturn].
	extractedParseTree := RBMethodNode 
				selector: #value
				arguments: #()
				body: (extractedParseTree isSequence 
						ifTrue: [extractedParseTree]
						ifFalse: 
							[RBSequenceNode temporaries: #()
								statements: (OrderedCollection with: extractedParseTree)]).
	extractedParseTree body temporaries isEmpty not 
		ifTrue: [extractedParseTree body temporaries: #()].
	extractedParseTree source: extractCode.
	parseTree := class parseTreeFor: selector.
	parseTree isNil 
		ifTrue: [self refactoringFailure: 'Could not parse ' , selector printString].
	subtree := isSequence 
				ifTrue: 
					[RBParseTreeSearcher 
						treeMatchingStatements: extractedParseTree body formattedCode
						in: parseTree]
				ifFalse: [RBParseTreeSearcher treeMatching: extractCode in: parseTree].
	subtree isNil 
		ifTrue: [self refactoringFailure: 'Could not extract code from method'].
	newCode := self methodDelimiter.
	isSequence 
		ifTrue: 
			[| stmts |
			stmts := extractedParseTree body statements.
			stmts isEmpty 
				ifFalse: 
					[stmts last isAssignment 
						ifTrue: 
							[| name |
							name := stmts last variable name.
							(self shouldExtractAssignmentTo: name) 
								ifFalse: 
									[newCode := '<1s> := <2s>' expandMacrosWith: name with: newCode.
									stmts at: stmts size put: stmts last value]]]].
	modifiedParseTree := isSequence 
				ifTrue: 
					[RBParseTreeRewriter 
						replaceStatements: subtree formattedCode
						with: newCode
						in: parseTree
						onInterval: extractionInterval]
				ifFalse: 
					[RBParseTreeRewriter 
						replace: subtree formattedCode
						with: newCode
						in: parseTree
						onInterval: extractionInterval]