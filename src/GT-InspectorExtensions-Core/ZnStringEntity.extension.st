Extension { #name : #ZnStringEntity }

{ #category : #'*GT-InspectorExtensions-Core' }
ZnStringEntity >> gtInspectorTextIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite text
		title: 'Contents';
		format: self string
]
