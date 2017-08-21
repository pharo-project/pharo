tests replacements
testCascadeReplacement
	| cascade |
	cascade := self parserClass parseExpression: 'self foo; bar; baz'.
	(cascade messages at: 2) 
		replaceWith: (self parserClass parseExpression: 'self bar: 2').
	self compare: cascade
		to: (self parserClass parseExpression: 'self foo; bar: 2; baz')