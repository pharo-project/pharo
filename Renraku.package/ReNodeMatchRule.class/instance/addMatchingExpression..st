adding
addMatchingExpression: aString
	"add a string of rewrite expression to be matched by rule"

	matchers add: (RBParser parseRewriteExpression: aString)