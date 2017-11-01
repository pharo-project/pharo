Extension { #name : #ZnRequestLine }

{ #category : #'*GT-InspectorExtensions-Core' }
ZnRequestLine >> gtInspectorTextIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite text
		title: 'HTTP';
		format: [ String streamContents: [ :out | self writeOn: out ] ]
]
