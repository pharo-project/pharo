tests
testRedundantCategoryAddition

	self class environment organization addCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.