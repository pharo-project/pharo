searching
searchedElement: anInteger

	searchedElement := anInteger.
	anInteger
		ifNil: [
			" just for redrawn "
			self vScrollValue: (self scrollValue y) ]
		ifNotNil: [ self vScrollValue: ((anInteger-1)/self getListSize) ]