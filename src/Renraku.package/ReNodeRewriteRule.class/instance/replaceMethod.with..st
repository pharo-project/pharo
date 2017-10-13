adding
replaceMethod: aString with: aRewriteString
	"add a 'from->to' pair of strings that represent a match method string to match and a rewrite method to replace the matched code"

	self addMatchingMethod: aString rewriteTo: aRewriteString