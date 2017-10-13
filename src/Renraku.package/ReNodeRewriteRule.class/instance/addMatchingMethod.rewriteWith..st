adding
addMatchingMethod: aString rewriteWith: aBlock
	"add 	a 'from->to' pair, first element of which is a rewrite method code in a form of a string that is used to match methods. The second parameter is a block that has to return a method ast which should replace the matched one. The block may accept 2 atguments: the matched ast, and a dictionary of wildcard variables mapping"

	matches
		at: (RBParser parseRewriteMethod: aString)
		put: (ReRewriteBlockWrapper on: aBlock)