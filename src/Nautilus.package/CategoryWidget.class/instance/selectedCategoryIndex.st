selection
selectedCategoryIndex

	^ self getCategories indexOf: self selectedCategory ifAbsent: [ 0 ]