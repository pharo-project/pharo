*GT-SpotterExtensions-Core
spotterForIncludesFor: aStep
	<spotterOrder: 1>
	aStep listProcessor
		title: 'Includes';
		allCandidates: [ self includes ];
		itemName: [ :item | item name ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true