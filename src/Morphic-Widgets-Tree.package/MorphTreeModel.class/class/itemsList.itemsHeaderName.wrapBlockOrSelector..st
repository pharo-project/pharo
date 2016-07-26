item handling
itemsList: itemsList itemsHeaderName: aName wrapBlockOrSelector: wrapBlockOrSelector

	^ self new
		wrapBlockOrSelector: wrapBlockOrSelector;
		rootItems: itemsList;
		headerLabel: aName;
		yourself