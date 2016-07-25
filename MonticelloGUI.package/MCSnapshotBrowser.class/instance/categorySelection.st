selecting
categorySelection
	^ categorySelection ifNil: [0] ifNotNil: [self visibleCategories indexOf: categorySelection]