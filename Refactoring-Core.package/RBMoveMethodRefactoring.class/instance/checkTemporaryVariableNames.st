preconditions
checkTemporaryVariableNames
	| varNames |
	varNames := parseTree allDefinedVariables.
	selfVariableName notNil ifTrue: [varNames add: selfVariableName].
	varNames do: 
			[:name | 
			moveToClasses do: 
					[:each | 
					(self canReferenceVariable: name in: each) 
						ifTrue: 
							[self refactoringError: ('<1p> already defines a variable called <2s>' 
										expandMacrosWith: each
										with: name)]]]