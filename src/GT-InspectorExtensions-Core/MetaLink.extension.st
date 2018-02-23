Extension { #name : #MetaLink }

{ #category : #'*GT-InspectorExtensions-Core' }
MetaLink >> gtInspectorDefinitonIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite text
		title: 'Definition';
		display: [ self definitionString ]
]
