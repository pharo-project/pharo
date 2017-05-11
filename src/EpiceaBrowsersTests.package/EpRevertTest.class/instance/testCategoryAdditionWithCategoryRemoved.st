tests
testCategoryAdditionWithCategoryRemoved

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	self setHeadAsInputEntry.

	self assert: (organization includesCategory: aCategory).
	self revertInputEntry.
	self deny: (organization includesCategory: aCategory).
