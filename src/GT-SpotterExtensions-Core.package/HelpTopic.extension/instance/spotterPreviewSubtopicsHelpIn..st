*GT-SpotterExtensions-Core
spotterPreviewSubtopicsHelpIn: aComposite
	<spotterPreview: 15>

	aComposite list
		title: [ self title ];
		display: #yourself;
		format: [ :aHelpTopic | GTSpotterItemBrick new 
				text: aHelpTopic title;
				icon: aHelpTopic gtTopicIcon;
				disable ];
		styled: [ :brick | brick ];
		entity: self subtopics.
		
	self flag: 'move "entity: self" somewhere else, maybe'