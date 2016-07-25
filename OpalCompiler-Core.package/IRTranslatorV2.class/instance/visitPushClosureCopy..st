visiting
visitPushClosureCopy: closure

	closure copiedValues do: [:name | 	
		gen pushTemp: (self currentScope indexForVarNamed: name).
	].
	gen mapBytesTo: closure.
	gen pushClosureCopyNumCopiedValues: closure copiedValues size 
			numArgs: closure arguments size 
			to: closure destination orderNumber.
	self pushScope: closure.
	gen pushNClosureTemps: closure definedTemps size

