tranforming
renameNode: aParseTree
	(aParseTree whoDefines: newName)
		ifNotNil: [ self refactoringError: newName asString , ' is already defined' ].
	(aParseTree allDefinedVariables includes: newName)
		ifTrue: [ self refactoringError: newName asString , ' is already defined' ].
	(RBParseTreeRewriter rename: oldName to: newName) executeTree: aParseTree