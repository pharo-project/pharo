ui
gtSpotterDependenciesFor: aStep
	<spotterOrder: 1>
	aStep listProcessor
		title: 'Example Dependencies';
		allCandidates: [ self example arguments ];
		itemName: [ :each | each gtDisplayString ];
		filter: GTFilterSubstring