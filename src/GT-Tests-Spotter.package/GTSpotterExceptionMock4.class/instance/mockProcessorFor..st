mocks
mockProcessorFor: aSpotterStep
	<spotterOrder: 1>
	aSpotterStep listProcessor
		allCandidates: [ #( 1 2 3 ) ];
		itemName: [ :item | item asString ];
		filter: GTSpotterExceptionMock4Filter " Exception while filtering "