tests interval
testLiteralIntevals
	| tree |
	tree := self parserClass parseExpression: '#(#a b #( c ))'.
	self assert: tree contents first start = 3.
	self assert: tree contents first stop = 4.
	self assert: tree contents last contents first start = 11