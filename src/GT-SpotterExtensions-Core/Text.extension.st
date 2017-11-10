Extension { #name : #Text }

{ #category : #'*GT-SpotterExtensions-Core' }
Text >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	^ aComposite text
		title: 'Contents';
		display: [ :item | item asString contractTo: 100 ];
		entity: self
]
