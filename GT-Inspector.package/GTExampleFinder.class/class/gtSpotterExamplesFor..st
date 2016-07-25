ui
gtSpotterExamplesFor: aStep
	<spotterOrder: 100>
	aStep listProcessor
		title: 'Examples';
		allCandidates: [ self gtExamplesContained ];
		itemName: [ :example | example gtDisplayString ];
		filter: GTFilterSubstring