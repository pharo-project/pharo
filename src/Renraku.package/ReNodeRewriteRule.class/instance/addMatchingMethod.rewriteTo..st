adding
addMatchingMethod: aString rewriteTo: aRewriteString
	"add a 'from->to' pair of strings that represent a match method string to match and a rewrite method to replace the matched mathod"

	matches
		at: (RBParser parseRewriteMethod: aString)
		put: (ReRewriteTreeWrapper on: (
			RBParser parseRewriteMethod: aRewriteString))