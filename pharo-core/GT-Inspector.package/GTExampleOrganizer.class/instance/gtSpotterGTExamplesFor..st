ui
gtSpotterGTExamplesFor: aStep
	<spotterOrder: 100>
	aStep listProcessor
		title: 'Examples';
		allCandidates: [ GTExampleFinder allGTExamples ];
		itemName: [ :example | example gtDisplayString ];
		filter: GTFilterSubstring