*GT-SpotterExtensions-Core
spotterPreviewCodeIn: aComposite
	<spotterPreview: 10>
	aComposite pharoMethod
		title: [ self gtDisplayString ];
		display: [:compiledMethod | 
			' ', compiledMethod sourceCode];
		smalltalkClass: [self methodClass];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'