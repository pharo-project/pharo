*GT-SpotterExtensions-Core
spotterPackageRepositoriesFor: aStep
	<spotterOrder: 10>
	aStep listProcessor
		title: 'Package repositories';
		allCandidates: [ self repositoryGroup repositories ];
		itemName: [ :item | item description ];
		actLogic: [ :item :step |
			step exit.
			self spotterCommit: self in: item ];
		filter: GTFilterSubstring