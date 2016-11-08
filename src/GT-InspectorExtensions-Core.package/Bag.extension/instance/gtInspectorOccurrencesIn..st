*GT-InspectorExtensions-Core
gtInspectorOccurrencesIn: composite
	<gtInspectorPresentationOrder: 1>  
	^ (composite fastTable)
		title: 'Occurrences';
		display: [ :aCollection | self asSet asOrderedCollection ];
		column: 'Item' 
			evaluated: [ :each | GTObjectPrinter new asTruncatedTextFrom: each ];
		column: 'Occurences' 
			evaluated: [ :each | self occurrencesOf: each ];
		beMultiple;
		"withSmalltalkSearch;"
		showOnly: 50;
		send: [ :result |
				result ];
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'.