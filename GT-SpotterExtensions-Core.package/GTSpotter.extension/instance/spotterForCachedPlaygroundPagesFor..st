*GT-SpotterExtensions-Core
spotterForCachedPlaygroundPagesFor: aStep
	<spotterOrder: 100>
	aStep listProcessor
			allCandidates: [ GTPlayBook instance pages ];
			title: 'Playground cached pages';
			itemName: [ :page | page content asString contractTo: 100 ];
			filter: GTFilterSubstring