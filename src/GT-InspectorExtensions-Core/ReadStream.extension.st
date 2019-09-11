Extension { #name : #ReadStream }

{ #category : #'*GT-InspectorExtensions-Core' }
ReadStream >> gtInspectorStreamStateIn: composite [
	<gtInspectorPresentationOrder: 50>
	composite text
		title: 'Contents';
		display: [ self contents ];
		initialize: [ :aPresentation | aPresentation selectionInterval: (1 to: self position) ]
]
