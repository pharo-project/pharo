*GT-InspectorExtensions-Core
gtInspectorStreamStateIn: composite
	<gtInspectorPresentationOrder: 50>
	(composite wrapper with: [ :w | 
		w title: 'Contents'.
		w show: [ :a | 
			a text
				display: [ self contents ];
				initialize: [ :aPresentation | 
					aPresentation selectionInterval: (1 to: self position) ] ] ])
		startOn: self