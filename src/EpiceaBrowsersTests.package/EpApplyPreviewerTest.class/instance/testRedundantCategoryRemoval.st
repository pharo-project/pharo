tests
testRedundantCategoryRemoval

	self class environment organization addCategory: classFactory defaultCategory.
	self class environment organization removeCategory: classFactory defaultCategory.
	self setHeadAsInputEntry.

	self assertEmptyPreviewLog.