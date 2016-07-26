visiting rb nodes
visitLiteralNode: aLiteralNode
	| value |
	value := aLiteralNode value.
	self 
		addStyle: (self literalStyleSymbol: value)
		attribute: (TextClassLink class: value class)
		forNode: aLiteralNode