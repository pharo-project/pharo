tests
testEntity2
	multiBundle add: bundle1.
	entity := multiBundle entity.
	self deny: entity isEmpty.
	self assert: entity parts size equals: 2.
	self assert: entity parts first fieldName equals: 'category'.
	self assert: entity parts second fieldName equals: 'data'.
	self assert: entity parts first fieldValue equals: #c1.
	self assert: entity parts second fieldValue equals: #[1 2 3].
	