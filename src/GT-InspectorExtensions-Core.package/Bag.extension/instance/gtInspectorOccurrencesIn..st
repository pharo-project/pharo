*GT-InspectorExtensions-Core
gtInspectorOccurrencesIn: composite
	<gtInspectorPresentationOrder: 1>  
	^ (composite fastTable)
		title: 'Occurrences';
		display: [ :aCollection | self asSet asOrderedCollection ];
		column: 'Item' 
			evaluated: [ :each | GTObjectPrinter asTruncatedTextFrom: each ];
		column: 'Occurences' 
			evaluated: [ :each | self occurrencesOf: each ]
			sortedBy: [ :a :b | (self occurrencesOf: a) > (self occurrencesOf: b)];
		beMultiple;
		"withSmalltalkSearch;"
		showOnly: 50;
		send: [ :result |
				result ];
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'.