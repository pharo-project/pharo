Extension { #name : #GLMPresentation }

{ #category : #'*GT-InspectorExtensions-Core' }
GLMPresentation >> gtInspectorBrowserIn: composite [
	<gtInspectorPresentationOrder: 40>  
	^ composite tabulator with: [ :t |
		t title: 'Live'.
		t column: #default.
		t transmit 
			to: #default; 
			transformed: [:x | self entity ]; 
			andShow: [:a | a custom: self copy ].
		t transmit from: #default port: #selection; toOutsidePort: #selection ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
GLMPresentation >> gtInspectorPanePortsIn: composite [
	<gtInspectorPresentationOrder: 50>
	(self pane gtInspectorPortsIn: composite)
		title: 'Pane ports'
]
