*GT-SpotterExtensions-Core
spotterPreviewStScriptIn: aComposite
	<spotterPreview: 10>
	
	(self extension = 'st' or: [self extension = 'ph']) 
		ifFalse: [ ^ self ].
	aComposite pharoMethod 
		title: [self gtDisplayString];
		display: #yourself;
		smalltalkClass: [ nil ];
		entity: (self readStreamDo: #contents)