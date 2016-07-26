selecting
categorySelection: aNumber
	categorySelection := aNumber = 0 ifFalse: [self visibleCategories at: aNumber].
	self classSelection: 0.
	self changed: #categorySelection;
		changed: #classList.
