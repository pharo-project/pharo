transformation
transform
	| rule |
	rule := RBParseTreeRewriter replaceLiteral: 42 with: 'meaning of life'.
	rule executeTree: ast.
	^ast