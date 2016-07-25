transforming
replaceSelfReferences
	| replacer |
	self needsToReplaceSelfReferences ifTrue: [
		replacer := RBParseTreeRewriter new.
		replacer replace: 'self' with: selfVariableName.
		self hasOnlySelfReturns ifTrue: 
			[replacer replace: '^self' with: '^self'].
		replacer executeTree: parseTree.
		parseTree := replacer tree].