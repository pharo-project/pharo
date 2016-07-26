accessing
model: aModel
	
	super model: aModel.
	self selectedCategory
		ifNotNil: [:cat | categoriesSelection at: cat put: true ]