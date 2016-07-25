tests
testCategoryAddition

	self assert: (self countLogEventsWith: EpCategoryAddition) equals: 0.

	self class environment organization addCategory: self categoryNameForTesting.
	
	self assert: (self countLogEventsWith: EpCategoryAddition) equals: 1.
	self assert: (self allLogEventsWith: EpCategoryAddition) first affectedPackageName equals: self categoryNameForTesting