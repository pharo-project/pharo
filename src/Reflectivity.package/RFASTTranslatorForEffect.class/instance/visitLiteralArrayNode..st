visitor-double dispatching
visitLiteralArrayNode: aLiteralNode
	"when visiting a literal array for effect, we could push it and then pop it, but we do nothing"
	methodBuilder addLiteral: aLiteralNode value