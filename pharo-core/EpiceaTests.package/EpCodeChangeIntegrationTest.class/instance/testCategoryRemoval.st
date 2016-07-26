tests
testCategoryRemoval

	self class environment organization addCategory: self categoryNameForTesting.

	self assert: (self countLogEventsWith: EpCategoryRemoval) equals: 0.

	self class environment organization removeCategory: self categoryNameForTesting.
	
	self assert: (self countLogEventsWith: EpCategoryRemoval) equals: 1.
	self assert: (self allLogEventsWith: EpCategoryRemoval) first affectedPackageName equals: self categoryNameForTesting