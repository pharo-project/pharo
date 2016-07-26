testing-adding
testAddReturn
	| tree return |
	tree := self parseExpression: '1. 2'.
	return := tree addReturn.
	self assert: tree statements last = return.
	self assert: (self parseExpression: '1. ^ 2') = tree.
	
	tree := self parseExpression: '3. ^ 4'.
	return := tree addReturn.
	self assert: tree statements last = return.
	self assert: (self parseExpression: '3. ^ 4') = tree