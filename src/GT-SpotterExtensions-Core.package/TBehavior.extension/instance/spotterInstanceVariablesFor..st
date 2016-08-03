*GT-SpotterExtensions-Core
spotterInstanceVariablesFor: aStep
	<spotterOrder: 20>
	self isTrait
		ifTrue: [ ^ self ].
	aStep listProcessor
		title: 'Instance variables';
		allCandidates: [ self classLayout allSlots ];
		itemName: [ :each | each name asString ];
		filter: GTFilterSubstring