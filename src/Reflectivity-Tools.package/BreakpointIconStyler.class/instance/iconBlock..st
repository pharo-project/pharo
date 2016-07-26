defaults
iconBlock: aNode
	^[ :seg | 
		Breakpoint removeFrom: aNode.
		seg delete]