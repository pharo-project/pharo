*GT-SpotterExtensions-Core
spotterForGroupsFor: aStep
	<spotterOrder: 2>
	aStep listProcessor
		title: 'Groups';
		allCandidates: [ self groups ];
		itemName: [ :item | item name ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true