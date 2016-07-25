mocks
mockProcessorFor: aSpotterStep
	<spotterOrder: 1>
	aSpotterStep listProcessor
		allCandidates: [ #( 1 2 3 ) ];
		itemName: [ :item | item / 0 ]; " Exception while serializing candidates "
		filter: GTFilterSubstring