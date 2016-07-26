visiting
visitNode: aNode
	"here we add the hook anotations and visit those AST parts for semantic analysis"
	super visitNode: aNode.
	aNode isMethod ifFalse: [self analyseForLinksForNodes: aNode]