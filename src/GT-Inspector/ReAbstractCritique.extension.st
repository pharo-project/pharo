Extension { #name : #ReAbstractCritique }

{ #category : #'*GT-Inspector' }
ReAbstractCritique >> gtInspectorDescriptionIn: composite [
	<gtInspectorPresentationOrder: 1>

	^ composite text
		title: 'Description';
		display: [ self description ]
]

{ #category : #'*GT-Inspector' }
ReAbstractCritique >> gtInspectorSourceCodeIn: composite [
	<gtInspectorPresentationOrder: 0>

	sourceAnchor gtInspectorAnchorIn: composite
	
		
]
