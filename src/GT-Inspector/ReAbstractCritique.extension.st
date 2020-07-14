Extension { #name : #ReAbstractCritique }

{ #category : #'*GT-Inspector' }
ReAbstractCritique >> gtInspectorDescriptionIn: composite [
	<gtInspectorPresentationOrder: 1>

	^ composite text
		title: 'Description';
		display: [ self description ]
]
