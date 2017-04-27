tests
testCategoryRename

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.

	self assert: inputEntry content class equals: EpCategoryRename.
	self deny: (organization includesCategory: aCategory).
	self assert: (organization includesCategory: anotherCategory).
	self revertInputEntry.
	self assert: (organization includesCategory: aCategory).
	self deny: (organization includesCategory: anotherCategory).
