set up
setupVariableTypesFor: aRefactoring toReturn: anObject 
	| options |
	options := aRefactoring options copy.
	options at: #variableTypes put: [:ref :ignore1 :ignore2 | anObject].
	aRefactoring options: options