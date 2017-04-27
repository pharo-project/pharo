tests
testBehaviorCategoryChange

	| aClass aCategory anotherCategory |
	aClass := classFactory newClass.
	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	self setHeadAsInputEntry.

	aClass category: aCategory.
	
	self assertOutputsAnEventWith: [:output |
		self assert: output class equals: EpBehaviorCategoryChange.
		self assert: output oldCategory equals: aCategory.
		self assert: output newCategory equals: anotherCategory.
		]
