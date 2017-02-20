gt-inspector-extension
gtInspectorItemsIn: composite
	<gtInspectorPresentationOrder: -1>  
	^ (composite table)
		title: 'Info';
		display: [ :anEvent | anEvent gtInspectorItems ];
		children: [ :each | 
			(each value isDictionary) 
				ifTrue: [ each value associations ] 
				ifFalse: [ (each value isArray and: [ each value allSatisfy: #isDictionary ])
						ifTrue: [ each value collectWithIndex: [:x :i | i -> x ] ]
						ifFalse: [ #() ] ] ];
		column: 'Key' 
			evaluated: [:each | GTObjectPrinter asTruncatedTextFrom: each key ];
		column: 'Value' 
			evaluated: [:each | GTObjectPrinter asTruncatedTextFrom: each value ];
		beMultiple;
		send: [ :selection | 
			(selection notNil and: [ selection size = 1 ]) 
				ifTrue: [ selection anyOne value ] 
				ifFalse: [ selection collect: #value ] ];
		showOnly: 50