*GT-SpotterExtensions-Core
spotterClassMethodsFor: aStep
	<spotterOrder: 15>
	aStep listProcessor
			title: 'Class methods';
			allCandidates: [ self classSide methods ];
			itemName: [ :method | method selector ];
			filter: GTFilterSubstring