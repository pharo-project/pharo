*GT-SpotterExtensions-Core
spotterForRepositoriesFor: aStep
	<spotterOrder: 87>
	aStep listProcessor
		title: 'Monticello Repositories';
		allCandidates: [ MCRepositoryGroup default repositories ];
		itemIcon: #systemIcon;
		wantsToDisplayOnEmptyQuery: false;
		filter: GTFilterSubstring