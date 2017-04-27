tests
testRedundantBehaviorCategoryChange

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	aClass category: aCategory. "Rollback"
	self setHeadAsInputEntry.
	
	self assertEmptyPreviewLog.