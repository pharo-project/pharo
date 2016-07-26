accessing-categories
categoryAt: anIndex
	^ categoriesSelected includes: (self categoryList at: anIndex ifAbsent: [ ^ false ]).