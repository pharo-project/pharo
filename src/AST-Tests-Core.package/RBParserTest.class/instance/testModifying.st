tests
testModifying
	| tree |
	tree := self parserClass  
				parseMethod: 'foo: a bar: b | c | self first. self second. a + b + c'.
	self deny: tree lastIsReturn.
	self deny: (tree body statements at: 2) isUsed.
	self assert: tree body statements last arguments first isUsed.
	self assert: (tree isLast: tree body statements last).
	self deny: (tree isLast: tree body statements first).
	self assert: (tree defines: 'a').
	self deny: (tree defines: 'c').
	self assert: (tree body defines: 'c').
	self deny: (tree body defines: 'a').
	tree
		addReturn;
		selector: #bar:foo:.
	(tree body)
		addTemporaryNamed: 'd';
		removeTemporaryNamed: 'c'.
	self compare: tree
		to: (self parserClass  
				parseMethod: 'bar: a foo: b | d | self first. self second. ^a + b + c').
	self 
		assert: ((tree argumentNames asSet)
				removeAll: #('a' 'b');
				yourself) isEmpty.
	self 
		assert: ((tree allDefinedVariables asSet)
				removeAll: #('a' 'b' 'd');
				yourself) isEmpty.
	tree := self parserClass  parseExpression: 'self foo: 0'.
	tree selector: #+.
	self compare: tree to: (self parserClass  parseExpression: 'self + 0').