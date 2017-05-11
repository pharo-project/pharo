tests
testCategoryRename

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.
	organization renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assert: inputEntry content class equals: EpCategoryRename.
	self assert: (organization includesCategory: aCategory).
	self deny: (organization includesCategory: anotherCategory).
	self applyInputEntry.
	self deny: (organization includesCategory: aCategory).
	self assert: (organization includesCategory: anotherCategory).
