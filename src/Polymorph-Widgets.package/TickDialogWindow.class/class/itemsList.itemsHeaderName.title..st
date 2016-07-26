instance creation
itemsList: itemsList itemsHeaderName: aName title: aTitle

	^ self basicNew
		itemsList: itemsList;
		itemsHeaderName: aName;
		initialize;
		title: aTitle;
		yourself