adding
replace: aString byEvaluating: aBlock
	"add 	a 'from->to' pair, first element of which is a rewrite expression in a form of a string that is used to match nodes. The second parameter is a block that has to return a node which should replace the matched one. The block may accept 2 atguments: the matched node, and a dictionary of wildcard variables mapping"

	self addMatchingExpression: aString rewriteWith: aBlock