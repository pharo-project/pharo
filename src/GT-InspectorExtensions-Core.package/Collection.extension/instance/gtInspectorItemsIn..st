*GT-InspectorExtensions-Core
gtInspectorItemsIn: composite
	<gtInspectorPresentationOrder: 0> 
	^ (composite fastList)
		title: 'Items';
		useCache;
		display: [ 
			"The FastTable renderer needs to access elements by index so we transform
			the collection to an OrderedCollection. Subclasses can override the #display
			block to provide a different behaviour. The previous renderer had this behaviour
			implemented in the renderer."
			self asOrderedCollection ];
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