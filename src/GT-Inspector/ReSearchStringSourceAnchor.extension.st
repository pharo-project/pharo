Extension { #name : #ReSearchStringSourceAnchor }

{ #category : #'*GT-Inspector' }
ReSearchStringSourceAnchor >> gtInspectorAnchorIn: composite [
	<gtInspectorPresentationOrder: 0>

	(super gtInspectorAnchorIn: composite)
		initialize: [ :p | p selectionInterval: self interval ]
]
