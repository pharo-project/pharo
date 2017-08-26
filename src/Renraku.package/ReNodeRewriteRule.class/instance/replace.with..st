adding
replace: aString with: aRewriteString
	"add a 'from->to' pair of strings that represent a match expression string to match and a rewrite expression to replace the matched node"

	self addMatchingExpression: aString rewriteTo: aRewriteString