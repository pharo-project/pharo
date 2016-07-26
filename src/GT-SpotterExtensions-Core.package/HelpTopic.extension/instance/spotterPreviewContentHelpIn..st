*GT-SpotterExtensions-Core
spotterPreviewContentHelpIn: aComposite
	<spotterPreview: 10>
	
	self hasSubtopics ifTrue: [ ^ self ].
	aComposite text
		title: [ self title ];
		display: [:aHelpTopic | 
			aHelpTopic contents];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'