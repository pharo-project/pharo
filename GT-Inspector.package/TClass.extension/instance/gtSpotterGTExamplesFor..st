*GT-Inspector-Examples-UI
gtSpotterGTExamplesFor: aStep
	<spotterOrder: 100>
	aStep listProcessor
		title: 'Examples';
		allCandidates: [ self gtExamples ];
		itemName: [ :example | example gtDisplayString ];
		filter: GTFilterSubstring