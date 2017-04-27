tests
testCategoryAdditionWithCategoryRemoved

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	self setHeadAsInputEntry.
	organization removeCategory: aCategory.

	self assert: inputEntry content class equals: EpCategoryAddition.
	self deny: (organization includesCategory: aCategory).
	self applyInputEntry.
	self assert: (organization includesCategory: aCategory).
