visiting
visitPatternWrapperBlockNode: aRBPatternWrapperBlockNode
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternWrapperBlockNode]