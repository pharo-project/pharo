testing-adding
testAddNodes
	| tree treeNodes |
	tree := self parseExpression: '1. 2'.
	treeNodes := tree addNodes: (self parseExpression: '3. 4') statements.
	self assert: (self parseExpression: '1. 2. 3. 4') = tree.
	self assert: (tree statements at: 3) = treeNodes first.
	self assert: (tree statements at: 4) = treeNodes last.
	
	tree := self parseExpression: '{ 1. 2 }'.
	treeNodes := tree addNodes: (self parseExpression: '3. 4') statements.
	self assert: (self parseExpression: '{ 1. 2. 3. 4 }') = tree.
	self assert: (tree statements at: 3) = treeNodes first.
	self assert: (tree statements at: 4) = treeNodes last