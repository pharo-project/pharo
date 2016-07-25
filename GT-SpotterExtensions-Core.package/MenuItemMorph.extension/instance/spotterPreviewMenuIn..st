*GT-SpotterExtensions-Core
spotterPreviewMenuIn: aComposite
	<spotterPreview: 10>
	
	aComposite list
		title: [ self contents ];
		display: #yourself;
		format: [ :item | 
			GTSpotterItemBrick new 
				text: item contents; 
				icon: item icon; 
				disable;
				yourself ];
		styled: [ :brick | brick ];
		entity: (self hasSubMenu ifFalse: [ {self} ] ifTrue: [ self subMenu allItems ]).
		
	self flag: 'move "entity: self" somewhere else, maybe'