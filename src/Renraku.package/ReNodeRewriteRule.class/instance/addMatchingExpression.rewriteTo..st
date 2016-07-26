adding
addMatchingExpression: aString rewriteTo: aRewriteString
	"add a 'from->to' pair of strings that represent a rewrite expression string to match and a rewrite expression to replace the matched node."

	matches
		at: (RBParser parseRewriteExpression: aString)
		put: (ReRewriteTreeWrapper on: (
			RBParser parseRewriteExpression: aRewriteString))