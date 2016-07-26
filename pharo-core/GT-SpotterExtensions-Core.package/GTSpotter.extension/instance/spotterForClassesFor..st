*GT-SpotterExtensions-Core
spotterForClassesFor: aStep
	<spotterOrder: 10>
	aStep listProcessor
			allCandidates: [ Smalltalk allClassesAndTraits ];
			title: 'Classes';
			filter: GTFilterSubstring;
			itemIcon: #systemIcon;
			keyBinding: $b meta;
			wantsToDisplayOnEmptyQuery: false