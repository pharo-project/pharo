Extension { #name : #ZnRequest }

{ #category : #'*GT-InspectorExtensions-Core' }
ZnRequest >> gtInspectorRequestIn: composite [
	<gtInspectorPresentationOrder: 25>
	(self requestLine gtInspectorTextIn: composite)
		title: 'Request'
]
