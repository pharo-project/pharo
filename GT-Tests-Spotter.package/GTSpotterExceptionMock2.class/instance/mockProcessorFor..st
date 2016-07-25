mocks
mockProcessorFor: aSpotterStep
	<spotterOrder: 1>
	aSpotterStep listProcessor
		allCandidates: [ nil ]; " Exception while collecting candidates "
		itemName: [ :item | item asString ];
		filter: GTFilterSubstring