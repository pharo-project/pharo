tests - ok
testRewriteDoesNotReuseOriginalNodes
	"Due to a bug in RBPatternVariableNode copyInContext method, creating a new astTree from the rewriter reused some nodes
	 of the original tree, this results in two trees sharing the identical nodes. The original AST now contained statement nodes
	that don't refer to the same parent (the method node) and that is wrong."

	| ast search replace |
	ast := RBParser
		parseMethod:
			'foo
self statement1.
self match.'.
	"all statement nodes have the same parent"
	self assert: ast statements first parent equals: ast statements last parent.
	search := '`msg
`@.statements.
`object match.'.
	replace := '`msg
`@.statements.
`object class.'.
	rewriter := RBParseTreeRewriter new.
	rewriter replaceMethod: search with: replace.
	rewriter executeTree: ast.
	"all statement nodes of the original AST still have the same parent"
	self assert: ast statements first parent equals: ast statements last parent