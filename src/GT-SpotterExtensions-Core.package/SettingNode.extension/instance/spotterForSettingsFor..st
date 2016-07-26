*GT-SpotterExtensions-Core
spotterForSettingsFor: aStep
	<spotterOrder: 0>
	self allChildren isEmpty ifTrue: [ ^ self ].
	aStep listProcessor
		title: 'Children';
		allCandidates: [ self allChildren ];
		itemName: [ :each | each spotterLabel ];
		filter: GTFilterSubstring