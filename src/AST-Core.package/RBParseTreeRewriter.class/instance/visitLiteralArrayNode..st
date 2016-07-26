visitor-double dispatching
visitLiteralArrayNode: aRBArrayLiteralNode
	aRBArrayLiteralNode contents: (aRBArrayLiteralNode contents collect: [ :each | self visitNode: each ])