code generation (closures)
ensureClosureAnalysisDone
	block blockExtent ifNil:
		[temporaries := block analyseArguments: arguments temporaries: temporaries rootNode: self]