tests
testCategoryRenameWithPreviousRollback

	| organization aCategory anotherCategory |
	organization := self class environment organization.
	aCategory := classFactory defaultCategory.
	anotherCategory := aCategory, '2'.
	organization addCategory: aCategory.
	organization renameCategory: aCategory toBe: anotherCategory.
	self setHeadAsInputEntry.
	organization renameCategory: anotherCategory toBe: aCategory. "Rollback"

	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpCategoryRename.
		self assert: output oldCategoryName equals: aCategory.
		self assert: output newCategoryName equals: anotherCategory.
		]
