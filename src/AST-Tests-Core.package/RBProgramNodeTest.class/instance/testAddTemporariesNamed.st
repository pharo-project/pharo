testing-adding
testAddTemporariesNamed
	| tree variables |
	tree := self parseExpression: '| a | a'.
	variables := tree addTemporariesNamed: #('b' 'c').
	self assert: variables first isVariable.
	self assert: variables first name = 'b'.
	self assert: variables second isVariable.
	self assert: variables second name = 'c'.
	self assert: tree temporaries second = variables first.
	self assert: tree temporaries last = variables second
	