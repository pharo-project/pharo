item handling
itemsList: itemsList itemsHeaderName: aName

	^ self new
		rootItems: itemsList;
		headerLabel: aName;
		yourself