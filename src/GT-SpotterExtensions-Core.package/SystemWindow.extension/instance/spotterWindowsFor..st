*GT-SpotterExtensions-Core
spotterWindowsFor: aStep
	<spotterOrder: 100>

	aStep listProcessor
			title: 'Windows';
			allCandidates: [ self class allSubInstances ];
			itemName: [ :window | '**', window label, '**' ];
			filter: GTFilterSubstring