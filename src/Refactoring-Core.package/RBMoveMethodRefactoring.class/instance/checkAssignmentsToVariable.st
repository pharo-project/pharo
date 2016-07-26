preconditions
checkAssignmentsToVariable
	| searcher |
	variable
		ifNotNil:
			[ 
			searcher := RBParseTreeSearcher new.
			searcher
				matches: variable , ' := `@object'
				do: [ :aNode :answer | true ].
			(searcher executeTree: parseTree initialAnswer: false)
				ifTrue:
					[ 
					self
						refactoringError:
							('Cannot move the method into <1s> since it is assigned'
								expandMacrosWith: variable) ] ]