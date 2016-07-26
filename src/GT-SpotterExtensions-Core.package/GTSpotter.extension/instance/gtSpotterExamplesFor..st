*GT-SpotterExtensions-Core
gtSpotterExamplesFor: aStep
	<todo: 'example streaming to speed up spotter!!!'>
	<spotterOrder: 41>
	aStep listProcessor
		title: 'Examples';
		allCandidates: [ GTExampleFinder allGTExamples ];
		itemName: [ :example | String streamContents: [:s | example gtDisplaySummaryOn: s] ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: false