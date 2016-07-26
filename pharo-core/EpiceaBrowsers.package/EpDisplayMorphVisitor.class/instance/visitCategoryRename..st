visitor
visitCategoryRename: aCategoryChange
	^ (self displayCategory: aCategoryChange oldCategoryName),
		(self displayOperation: '->'),
		(self displayCategory: aCategoryChange newCategoryName )