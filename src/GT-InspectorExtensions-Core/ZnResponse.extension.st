Extension { #name : #ZnResponse }

{ #category : #'*GT-InspectorExtensions-Core' }
ZnResponse >> gtInspectorResponseIn: composite [
	<gtInspectorPresentationOrder: 25>
	(self statusLine gtInspectorTextIn: composite)
		title: 'Response'
]
