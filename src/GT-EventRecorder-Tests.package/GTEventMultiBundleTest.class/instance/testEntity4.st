tests
testEntity4
	multiBundle add: bundle1; add: bundle2; add: bundle3.
	entity := multiBundle entity.
	self deny: entity isEmpty.
	self assert: entity parts size equals: 6.
	self assert: entity parts first fieldName equals: 'category'.
	self assert: entity parts second fieldName equals: 'data'.
	self assert: entity parts third fieldName equals: 'category'.
	self assert: entity parts fourth fieldName equals: 'data'.
	self assert: entity parts fifth fieldName equals: 'category'.
	self assert: entity parts sixth fieldName equals: 'data'.
	self assert: entity parts first fieldValue equals: #c1.
	self assert: entity parts second fieldValue equals: #[1 2 3].
	self assert: entity parts third fieldValue equals: #c1.
	self assert: entity parts fourth fieldValue equals: #[4 5 6].
	self assert: entity parts fifth fieldValue equals: #c2.
	self assert: entity parts sixth fieldValue equals: #[7 8 9].
