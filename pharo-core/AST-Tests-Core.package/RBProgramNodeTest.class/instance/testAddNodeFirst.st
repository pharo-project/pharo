testing-adding
testAddNodeFirst
	| tree treeNode |
	tree := self parseExpression: '2. 3'.
	treeNode := tree addNodeFirst: (self parseExpression: '1').
	self assert: (self parseExpression: '1. 2. 3') = tree.
	self assert: tree statements first = treeNode.
	
	tree := self parseExpression: '{ 2. 3 }'.
	treeNode := tree addNodeFirst: (self parseExpression: '1').
	self assert: (self parseExpression: '{ 1. 2. 3 }') = tree.
	self assert: tree statements first = treeNode