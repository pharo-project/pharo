testing-adding
testAddTemporaryNamed
	| tree variable |
	tree := self parseExpression: '| a | a'.
	variable := tree addTemporaryNamed: 'b'.
	self assert: variable isVariable.
	self assert: variable name = 'b'.
	self assert: tree temporaries last = variable