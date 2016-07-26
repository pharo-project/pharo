*GT-SpotterExtensions-Core
spotterSuperclassesFor: aStep
	<spotterOrder: 40>
	aStep listProcessor
			title: 'All superclasses';
			allCandidates: [ self allSuperclasses ];
			itemIcon: #systemIcon;
			filter: GTFilterSubstring