*qualityAssistant
spotterQACritiquesFor: aStep
	<spotterOrder: 0>
	| eProperties |
	
	"A way to disable plugin from prefs"
	eProperties := QASettings spotterPlugin
		ifTrue: [ self externalProperties asArray ]
		ifFalse: [ #() ].

	
	
	aStep listProcessor
		title: 'QA Critics';
		items: [ eProperties ];
		itemName: #title;
		itemIcon: #icon;
		filter: GTFilterSubstring