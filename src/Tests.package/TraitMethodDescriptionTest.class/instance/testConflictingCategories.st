testing
testConflictingCategories
	| t7 t8 |
	self t2 compile: 'm11' classified: #catY.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t5 organization categoryOfElement: #m11) equals: #cat1.
	t7 := self createTraitNamed: #T7 uses: self t1 + self t2.
	self assert: (t7 organization categoryOfElement: #m11) 
				equals: Protocol ambiguous.
	self t1 removeSelector: #m11.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t5 organization categoryOfElement: #m11) equals: #catY.
	self assert: (t7 organization categoryOfElement: #m11) equals: #catY.
	self assert: (t7 organization categories includes: Protocol ambiguous) not.
	self t1 compile: 'm11' classified: #cat1.
	t8 := self createTraitNamed: #T8 uses: self t1 + self t2.
	t8 organization classify: #m11 under: #cat1.
	self t1 organization classify: #m11 under: #catZ.
	self assert: (self t4 organization categoryOfElement: #m11) equals: #catX.
	self assert: (self t5 organization categoryOfElement: #m11) equals: #catY.
	self assert: (t8 organization categoryOfElement: #m11) equals: #catZ