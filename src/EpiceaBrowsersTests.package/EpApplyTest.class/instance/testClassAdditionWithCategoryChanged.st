tests
testClassAdditionWithCategoryChanged

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	self setHeadAsInputEntry.

	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	
	self assert: aClass category equals: anotherCategory.
	self applyInputEntry.
	self assert: aClass category equals: aCategory