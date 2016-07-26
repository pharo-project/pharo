visiting
visitPatternBlockNode: aRBPatternBlockNode 
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternBlockNode]