*GT-SpotterExtensions-Core
spotterForPackagesFor: aStep
	<spotterOrder: 3>
	aStep listProcessor
		title: 'Packages';
		allCandidates: [ self packages ];
		itemName: [ :item | item file ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true