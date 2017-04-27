tests
testBehaviorCategoryChange

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	self setHeadAsInputEntry.
	
	self assert: (inputEntry content isKindOf: EpBehaviorCategoryChange).
	self assert: aClass category equals: anotherCategory.
	self revertInputEntry.
	self assert: aClass category equals: aCategory.
