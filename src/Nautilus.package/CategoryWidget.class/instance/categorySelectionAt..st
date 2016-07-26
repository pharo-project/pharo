selection
categorySelectionAt: anIndex

	| elt |
	elt := self getCategories at: anIndex ifAbsent: [ nil ].
	^ categoriesSelection at: elt ifAbsent: [ false ].