*GT-SpotterExtensions-Core
spotterWorldMenuFor: aStep
	<spotterOrder: 9>
	
	^ (World worldMenu spotterItemsFor: aStep)
		candidatesLimit: 15;
		allCandidates: [ World worldMenu allItems ];
		title: 'Menu'