*GT-InspectorExtensions-Core
gtInspectorItemsIn: composite
	<gtInspectorPresentationOrder: 0>  
	^ (composite fastList)
		title: 'Items';
		display: [ :aCollection | self ];
		beMultiple;	
		format: [ :each | GTObjectPrinter asTruncatedTextFrom: each ];
		send: [ :result | result isNil 
			ifTrue:[nil]
			ifFalse:[ (result size = 1) 
				ifTrue: [result anyOne] 
				ifFalse: [self species withAll: result]]]
		"withSmalltalkSearch;	
		showOnly: 50;
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'."