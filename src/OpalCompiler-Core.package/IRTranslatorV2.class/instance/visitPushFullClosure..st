visiting
visitPushFullClosure: fullBlock

	fullBlock copiedValues do: [:name | 	
		gen pushTemp: (self currentScope indexForVarNamed: name)].
	gen mapBytesTo: fullBlock.
	gen pushFullBlockClosure: fullBlock
