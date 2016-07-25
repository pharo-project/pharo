*GT-SpotterExtensions-Core
spotterExtensionsFor: aStep
	<spotterOrder: 50>
	aStep listProcessor
			allCandidates: [ self inspectorExtendingMethods ];
			title: 'Extensions';
			itemName:[ :each | each gtDisplayString ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: true