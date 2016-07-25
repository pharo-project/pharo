tests replacements
testCascadeReplacement
	| cascade |
	cascade := RBParser parseExpression: 'self foo; bar; baz'.
	(cascade messages at: 2) 
		replaceWith: (RBParser parseExpression: 'self bar: 2').
	self compare: cascade
		to: (RBParser parseExpression: 'self foo; bar: 2; baz')