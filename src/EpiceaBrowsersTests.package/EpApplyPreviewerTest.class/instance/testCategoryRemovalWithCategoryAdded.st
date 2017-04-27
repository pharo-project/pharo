tests
testCategoryRemovalWithCategoryAdded

	self class environment organization addCategory: classFactory defaultCategory.
	self class environment organization removeCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.

	self class environment organization addCategory: classFactory defaultCategory.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpCategoryRemoval.
		self assert: output categoryName equals: classFactory defaultCategory.
		]