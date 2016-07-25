tranforming
renameNode: aParseTree 
	(aParseTree whoDefines: newName) notNil 
		ifTrue: [self refactoringError: newName , ' is already defined'].
	(aParseTree allDefinedVariables includes: newName) 
		ifTrue: [self refactoringError: newName , ' is already defined'].
	(RBParseTreeRewriter rename: oldName to: newName) executeTree: aParseTree