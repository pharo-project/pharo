*GT-SpotterExtensions-Core
spotterForKeysFor: aStep
	<spotterOrder: 15>
	aStep listProcessor
			title: 'Keys';
			allCandidates: [ self keys ];
			candidatesLimit: 5;
			filter: GTFilterSubstring