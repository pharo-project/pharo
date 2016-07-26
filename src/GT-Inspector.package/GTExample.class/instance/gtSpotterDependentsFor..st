ui
gtSpotterDependentsFor: aStep
	<spotterOrder: 101>
	self dependents ifEmpty: [ ^ self ].
	aStep listProcessor
		title: 'Example Dependents';
		allCandidates: [ self dependents ];
		itemName: [ :example | example gtDisplayString ];
		filter: GTFilterSubstring