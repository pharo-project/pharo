Extension { #name : #Bag }

{ #category : #'*GT-InspectorExtensions-Core' }
Bag >> gtInspectorItemsIn: composite [
	"Disable the Items presentation for a bag.
	This avoid showing all duplicated items."
]

{ #category : #'*GT-InspectorExtensions-Core' }
Bag >> gtInspectorOccurrencesIn: composite [
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
]
