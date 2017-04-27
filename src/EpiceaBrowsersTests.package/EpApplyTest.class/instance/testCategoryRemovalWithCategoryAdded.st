tests
testCategoryRemovalWithCategoryAdded

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	organization removeCategory: aCategory.
	self setHeadAsInputEntry.
	organization addCategory: aCategory.

	self assert: inputEntry content class equals: EpCategoryRemoval.
	self assert: (organization includesCategory: aCategory).
	self applyInputEntry.
	self deny: (organization includesCategory: aCategory).
