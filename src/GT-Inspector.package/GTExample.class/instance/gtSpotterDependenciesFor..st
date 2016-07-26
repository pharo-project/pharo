ui
gtSpotterDependenciesFor: aStep
	<spotterOrder: 100>
	self arguments ifEmpty: [ ^ self ].
	aStep listProcessor
		title: 'Example Dependencies';
		allCandidates: [ self arguments ];
		itemName: [ :example | example gtDisplayString ];
		filter: GTFilterSubstring