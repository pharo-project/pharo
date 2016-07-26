selection
selectedCategoryIndex: anInteger

	| anObject |
	anObject := self getCategories at: anInteger ifAbsent: [ nil ].
	self model selectedCategory: anObject.
	
	self changed: #selectedCategoryIndex.
	self model categorySelectionChanged.
	self model changed: #currentHistoryIndex.