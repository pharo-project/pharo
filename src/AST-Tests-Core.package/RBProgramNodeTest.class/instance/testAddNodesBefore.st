testing-adding
testAddNodesBefore
	| tree treeNodes |
	tree := self parseExpression: '1. 4'.
	treeNodes := tree 
		addNodes: (self parseExpression: '2. 3') statements
		before: tree statements last.
	self assert: (self parseExpression: '1. 2. 3. 4') = tree.
	self assert: (tree statements at: 2) = treeNodes first.
	self assert: (tree statements at: 3) = treeNodes last.
	
	tree := self parseExpression: '{ 1. 4 }'.
	treeNodes := tree 
		addNodes: (self parseExpression: '2. 3') statements
		before: tree statements last.
	self assert: (self parseExpression: '{ 1. 2. 3. 4 }') = tree.
	self assert: (tree statements at: 2) = treeNodes first.
	self assert: (tree statements at: 3) = treeNodes last