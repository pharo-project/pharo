ui
gtSpotterDependentsFor: aStep
	<spotterOrder: 2>
	aStep listProcessor
		title: 'Example Dependents';
		allCandidates: [ self example dependents ];
		itemName: [ :each | each gtDisplayString ];
		filter: GTFilterSubstring