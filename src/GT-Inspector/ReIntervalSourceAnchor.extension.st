Extension { #name : #ReIntervalSourceAnchor }

{ #category : #'*GT-Inspector' }
ReIntervalSourceAnchor >> gtInspectorAnchorIn: composite [
	<gtInspectorPresentationOrder: 0>

	(super gtInspectorAnchorIn: composite)
		initialize: [ :p | p selectionInterval: interval ]
]
