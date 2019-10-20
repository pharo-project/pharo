Extension { #name : #ReAbstractCritique }

{ #category : #'*GT-Spotter' }
ReAbstractCritique >> spotterPreviewRationaleIn: aComposite [
	<spotterPreview: 5>
	aComposite text
		title: [ self title ];
		display: [ self description ];
		entity: self
]
