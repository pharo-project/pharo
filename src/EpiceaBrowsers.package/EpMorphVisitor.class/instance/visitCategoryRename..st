visitor
visitCategoryRename: aCategoryChange

	^	(self displayOperation: 'Rename tag'),
		(self displayString: aCategoryChange oldCategoryName),
		(self displayOperation: 'as'),
		(self displayString: aCategoryChange newCategoryName )