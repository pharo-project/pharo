accessing
executeTree: aParseTree 
	"Save our current context, in case someone is performing another search inside a match."

	| oldContext |
	oldContext := context.
	context := SmallDictionary new.
	self visitNode: aParseTree.
	context := oldContext.
	^answer