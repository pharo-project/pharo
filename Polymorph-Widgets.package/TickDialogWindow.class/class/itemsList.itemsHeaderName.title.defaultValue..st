instance creation
itemsList: itemsList itemsHeaderName: aName title: aTitle defaultValue: aBoolean
	^ self basicNew
		itemsList: itemsList;
		itemsHeaderName: aName;
		defaultValue: aBoolean;
		initialize;
		title: aTitle;
		yourself