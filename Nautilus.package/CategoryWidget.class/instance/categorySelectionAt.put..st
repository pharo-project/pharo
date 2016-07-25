selection
categorySelectionAt: anIndex put: aBoolean
	| elt |
	
	elt := self getCategories at: anIndex ifAbsent: [ nil ].
	categoriesSelection at: elt put: aBoolean.
	self model categorySelectionChanged.
	^ aBoolean