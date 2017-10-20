*GT-SpotterExtensions-Core
spotterMethodsFor: aStep
	<spotterOrder: 10>
	aStep listProcessor
			title: 'Instance methods';
			allCandidates: [ self methods ];
			itemName: [ :method | method selector ];
			filter: GTFilterSubstring