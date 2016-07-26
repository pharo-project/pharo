visitor-double dispatching
visitLiteralNode: aLiteralNode
	"when visiting a literal for effect, we could push it and then pop it, but we do nothing"
	methodBuilder addLiteral: aLiteralNode value