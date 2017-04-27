tests
testCategoryAdditionWithCategoryRemoved

	self class environment organization addCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.
	self class environment organization removeCategory: classFactory defaultCategory.

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpCategoryAddition.
		self assert: output categoryName equals: classFactory defaultCategory.
		]