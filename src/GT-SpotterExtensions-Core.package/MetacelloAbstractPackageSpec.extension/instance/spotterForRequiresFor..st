*GT-SpotterExtensions-Core
spotterForRequiresFor: aStep
	<spotterOrder: 2>
	aStep listProcessor
		title: 'Requires';
		allCandidates: [ self requires ];
		itemName: [ :item | item name ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true