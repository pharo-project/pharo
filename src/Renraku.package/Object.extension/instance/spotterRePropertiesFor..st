*renraku
spotterRePropertiesFor: aStep
	<spotterOrder: 0>
	
	ReSettings spotterPlugin ifFalse: [ ^ self ].
	
	aStep listProcessor
		title: 'Renraku Properties';
		items: [ self externalProperties asArray ];
		itemName: #title;
		itemIcon: #icon;
		filter: GTFilterSubstring