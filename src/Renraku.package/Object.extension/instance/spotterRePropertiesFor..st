*renraku
spotterRePropertiesFor: aStep
	<spotterOrder: 0>
	| eProperties |
	
	"A way to disable plugin from prefs"
	eProperties := ReSettings spotterPlugin
		ifTrue: [ self externalProperties asArray ]
		ifFalse: [ #() ].

	
	
	aStep listProcessor
		title: 'Renraku Properties';
		items: [ eProperties ];
		itemName: #title;
		itemIcon: #icon;
		filter: GTFilterSubstring