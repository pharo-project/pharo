Extension { #name : #AbstractMorphicAdapter }

{ #category : #'*GT-InspectorExtensions-Core' }
AbstractMorphicAdapter >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 30>
	
	self widgetDo: [ :w | w gtInspectorMorphIn: composite ]
]
