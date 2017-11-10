Extension { #name : #ReadStream }

{ #category : #'*GT-InspectorExtensions-Core' }
ReadStream >> gtInspectorStreamStateIn: composite [
	<gtInspectorPresentationOrder: 50>
	(composite wrapper with: [ :w | 
		w title: 'Contents'.
		w show: [ :a | 
			a text
				display: [ self contents ];
				initialize: [ :aPresentation | 
					aPresentation selectionInterval: (1 to: self position) ] ] ])
		startOn: self
]
