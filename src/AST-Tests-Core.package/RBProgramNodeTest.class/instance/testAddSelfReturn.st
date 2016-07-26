testing-adding
testAddSelfReturn
	| tree return |
	tree := self parseExpression: '1. 2'.
	return := tree addSelfReturn.
	self assert: tree statements last = return.
	self assert: (self parseExpression: '1. 2. ^ self') = tree.
	
	tree := self parseExpression: '3. ^ 4'.
	return := tree addSelfReturn.
	self assert: tree statements last = return.
	self assert: (self parseExpression: '3. ^ 4') = tree