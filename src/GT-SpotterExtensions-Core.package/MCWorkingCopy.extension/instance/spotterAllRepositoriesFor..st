*GT-SpotterExtensions-Core
spotterAllRepositoriesFor: aStep
	<spotterOrder: 20>
	aStep listProcessor
		title: 'All other repositories';
		allCandidates: [ MCRepositoryGroup default repositories \ self repositoryGroup repositories ];
		itemName: [ :item | item description ];
		actLogic: [ :item :step | 
			step exit.
			self spotterCommit: self in: item ];
		filter: GTFilterSubstring