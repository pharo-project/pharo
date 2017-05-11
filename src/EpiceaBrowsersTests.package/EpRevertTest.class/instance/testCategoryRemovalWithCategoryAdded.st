tests
testCategoryRemovalWithCategoryAdded

	| organization aCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	organization addCategory: aCategory.
	organization removeCategory: aCategory.
	self setHeadAsInputEntry.

	self deny: (organization includesCategory: aCategory).
	self revertInputEntry.
	self assert: (organization includesCategory: aCategory).
