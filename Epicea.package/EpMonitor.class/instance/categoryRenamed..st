announcement handling
categoryRenamed: aCategoryRenamed
	| aChange |
	aChange := EpCategoryRename
		oldName: aCategoryRenamed oldCategoryName
		newName: aCategoryRenamed newCategoryName.
	self addEvent: aChange.