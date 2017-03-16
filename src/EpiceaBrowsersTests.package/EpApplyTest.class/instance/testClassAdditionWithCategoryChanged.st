tests
testClassAdditionWithCategoryChanged

	| aClass anEntry aCategory anotherCategory |
	aClass := classFactory newClass.
	anEntry := monitor log head.

	aCategory := aClass category.
	anotherCategory := aCategory, '2'.
	aClass category: anotherCategory.
	
	self assert: aClass category equals: anotherCategory.
	self apply: { anEntry }.
	self assert: aClass category equals: aCategory