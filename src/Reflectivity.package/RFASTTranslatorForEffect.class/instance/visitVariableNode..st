visitor-double dispatching
visitVariableNode: aVariableNode
	"when visiting a variable for effect, we could push it and then pop it, but we do nothing"
	| binding |
	binding := aVariableNode binding.
	(binding isLiteralVariable or: [ binding isUndeclared ])
		ifTrue: [ methodBuilder addLiteral: binding assoc ]