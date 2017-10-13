accessing
executeTree: aParseTree 
	"Replace the argument node based on the replace rules.
	Return false when no transformation has been applied, and true when a transformation occured."
	
	"here is a little script showing a possible way to use executeTree: 
	| rewriter node |
	rewriter := RBParseTreeRewriter new.
	rewriter replace: 'self halt' with: 'self dormatHalt'.
	node := (ProtoObjectTest>>#testIfNil) parseTree.
	rewriter executeTree: node.
	^ node formattedCode
"


	| oldContext |
	oldContext := context.
	context := SmallDictionary new.
	answer := false.
	tree := self visitNode: aParseTree.
	context := oldContext.
	^answer