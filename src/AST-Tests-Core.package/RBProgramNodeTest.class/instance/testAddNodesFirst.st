testing-adding
testAddNodesFirst
	| tree treeNodes |
	tree := self parseExpression: '3. 4'.
	treeNodes := tree addNodesFirst: (self parseExpression: '1. 2') statements.
	self assert: (self parseExpression: '1. 2. 3. 4') = tree.
	self assert: (tree statements at: 1) = treeNodes first.
	self assert: (tree statements at: 2) = treeNodes last.
	
	tree := self parseExpression: '{ 3. 4 }'.
	treeNodes := tree addNodesFirst: (self parseExpression: '1. 2') statements.
	self assert: (self parseExpression: '{ 1. 2. 3. 4 }') = tree.
	self assert: (tree statements at: 1) = treeNodes first.
	self assert: (tree statements at: 2) = treeNodes last