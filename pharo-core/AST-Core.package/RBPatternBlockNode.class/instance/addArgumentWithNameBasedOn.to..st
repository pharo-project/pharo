matching
addArgumentWithNameBasedOn: aString to: aRBBlockNode 
	| name index vars |
	name := aString.
	vars := aRBBlockNode allDefinedVariables.
	index := 0.
	[vars includes: name] whileTrue: 
			[index := index + 1.
			name := name , index printString].
	aRBBlockNode 
		arguments: (aRBBlockNode arguments copyWith: (RBVariableNode named: name))