Extension { #name : #GLMBrowser }

{ #category : #'*GT-InspectorExtensions-Core' }
GLMBrowser >> gtInspectorTransmissionsIn: composite [
	<gtInspectorPresentationOrder: 35>  
	^ composite list 
		title: 'Transmissions';
		display: [ self transmissions ]
]
