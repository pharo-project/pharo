Extension { #name : #ZnHeaders }

{ #category : #'*GT-InspectorExtensions-Core' }
ZnHeaders >> gtInspectorHeadersIn: composite [ 
	<gtInspectorPresentationOrder: 30>
	^ (self headers gtInspectorItemsIn: composite) 
		display: [ :anObject | self headers associations ];
		title: 'Items'
]
