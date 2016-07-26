emitting code
emitArgument: aBuilder context: aContext
	aBuilder pushInstVar: (aContext methodClass instVarIndexFor: self argName).