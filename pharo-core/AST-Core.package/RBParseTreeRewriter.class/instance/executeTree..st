accessing
executeTree: aParseTree 
	| oldContext |
	oldContext := context.
	context := SmallDictionary new.
	answer := false.
	tree := self visitNode: aParseTree.
	context := oldContext.
	^answer