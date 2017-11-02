Extension { #name : #SmallDictionary }

{ #category : #'*GT-InspectorExtensions-Core' }
SmallDictionary >> gtInspectorItemsIn: composite [
	^ (composite fastTable)
		title: 'Items';
		display: [ self associations ];
		"children: [ :each | 
			(each value isDictionary) 
				ifTrue: [ each value associations ] 
				ifFalse: [ (each value isArray and: [ each value allSatisfy: #isDictionary ])
						ifTrue: [ each value collectWithIndex: [:x :i | i -> x ] ]
						ifFalse: [ #() ] ] ];"
		column: 'Key' 
			evaluated: [:each | GTObjectPrinter asTruncatedTextFrom: each key ];
		column: 'Value' 
			evaluated: [:each | GTObjectPrinter asTruncatedTextFrom: each value ];
		selectionAct: [ :table |
			table rawSelection do: [ :assoc | self removeKey: assoc key ].
			table update ]
			entitled: 'Remove item(s)';
		selectionPopulate: #selection 
			entitled: 'Open key'
			with: [ :table | (table rawSelection collect: #key) gtInspectorInterestingObject ];
		beMultiple;
		send: [ :selection | selection isNil 
			ifTrue:[nil]
			ifFalse:[ (selection size = 1 ) 
				ifTrue: [ selection anyOne value ] 
				ifFalse: [ selection collect: #value ] ]]
		"withSmalltalkSearch;
		showOnly: 50;
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'"
]
