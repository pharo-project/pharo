announcement handling
categoryAdded: aCategoryAdded
	| aChange |
	aChange := EpCategoryAddition
		named: aCategoryAdded categoryName.
	self addEvent: aChange.