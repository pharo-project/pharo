selection
categorySelectionNamed: aString put: aBoolean
	
	categoriesSelection at: aString put: aBoolean.
	self model categorySelectionChanged.
	^ aBoolean