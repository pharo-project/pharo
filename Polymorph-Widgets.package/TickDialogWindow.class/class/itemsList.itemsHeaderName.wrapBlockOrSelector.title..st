instance creation
itemsList: itemsList itemsHeaderName: aName wrapBlockOrSelector: aBlockOrSelector title: aTitle
	^ self basicNew
		itemsList: itemsList;
		itemsHeaderName: aName;
		wrapBlockOrSelector: aBlockOrSelector;
		initialize;
		title: aTitle;
		yourself