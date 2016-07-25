instance creation
itemsList: itemsList itemsHeaderName: aName wrapBlockOrSelector: aBlockOrSelector title: aTitle defaultValue: aBoolean
	^ self basicNew
		itemsList: itemsList;
		itemsHeaderName: aName;
		wrapBlockOrSelector: aBlockOrSelector;
		defaultValue: aBoolean;
		initialize;
		title: aTitle;
		yourself