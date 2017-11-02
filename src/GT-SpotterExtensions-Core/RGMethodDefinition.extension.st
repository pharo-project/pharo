Extension { #name : #RGMethodDefinition }

{ #category : #'*GT-SpotterExtensions-Core' }
RGMethodDefinition >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	aComposite pharoMethod
		title: [ self gtDisplayString ];
		display: [ :methodDefinition | 
			' ', methodDefinition sourceCode];
		smalltalkClass: [self methodClass];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]
