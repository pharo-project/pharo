*GT-SpotterExtensions-Core
spotterPreviewItemsIn: aComposite
	<spotterPreview: 10>
	
	self isDirectory ifFalse: [ ^ self ].
	aComposite list
		title: [ self gtDisplayString ];
		display: #yourself;
		format: [ :folder | 
			GTSpotterItemBrick new 
				text: folder basename; 
				icon: (folder isDirectory 
								ifTrue: [Smalltalk ui theme icons glamorousFolder]
								ifFalse: [Smalltalk ui theme icons glamorousBrowse] ); 
				disable; 
				yourself ];
		styled: [ :brick | brick ];
		entity: self directories, self files.
		
	self flag: 'move "entity: self" somewhere else, maybe'