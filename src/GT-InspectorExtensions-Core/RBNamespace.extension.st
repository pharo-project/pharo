Extension { #name : #RBNamespace }

{ #category : #'*GT-InspectorExtensions-Core' }
RBNamespace >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 0>
	self changes isNil ifTrue: [ ^ self ].
	self changes gtInspectorPreviewIn: composite
]
