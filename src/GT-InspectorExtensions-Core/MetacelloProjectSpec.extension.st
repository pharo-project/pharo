Extension { #name : #MetacelloProjectSpec }

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloProjectSpec >> gtInspectorPackagesIn: composite [
	<gtInspectorPresentationOrder: 40>
	self version gtInspectorPackagesIn: composite
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloProjectSpec >> gtInspectorProjectsIn: composite [
	<gtInspectorPresentationOrder: 40>
	self version gtInspectorProjectsIn: composite
]
