*GT-SpotterExtensions-Core
spotterPreviewItemsIn: aComposite
	<spotterPreview: 10>
	
	self isDirectory ifFalse: [ ^ self ].
	aComposite list
		title: [ self gtDisplayString ];
		display: #yourself;
		format: [ :folder | 
			|iconName |			
			iconName := folder isDirectory 
				ifTrue: [ #glamorousFolder ]
				ifFalse: [ #glamorousBrowse ].
			
			GTSpotterItemBrick new 
				text: folder basename; 
				icon: (self iconNamed: iconName); 
				disable; 
				yourself ];
		styled: [ :brick | brick ];
		entity: self directories, self files.
		
	self flag: 'move "entity: self" somewhere else, maybe'