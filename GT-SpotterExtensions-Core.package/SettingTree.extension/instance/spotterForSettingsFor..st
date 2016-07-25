*GT-SpotterExtensions-Core
spotterForSettingsFor: aStep
	<spotterOrder: 0>
	aStep listProcessor
		title: 'Settings';
		allCandidates: [ self nodeList ];
		itemName: [ :each | each spotterLabel ];
		filter: GTFilterSubstring