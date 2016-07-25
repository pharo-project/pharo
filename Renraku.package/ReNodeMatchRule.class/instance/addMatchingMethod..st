adding
addMatchingMethod: aString
	"add a string of rewrite expression which should be parsed as a method"

	matchers add: (RBParser parseRewriteMethod: aString)