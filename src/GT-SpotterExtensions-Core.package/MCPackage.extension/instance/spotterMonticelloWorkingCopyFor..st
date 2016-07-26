*GT-SpotterExtensions-Core
spotterMonticelloWorkingCopyFor: aStep
	<spotterOrder: 1>
	aStep listProcessor
		title: 'Monticello Working Copy';
		allCandidates: [ Array with: self workingCopy ];
		itemIcon: [ Smalltalk ui icons iconNamed: #versionControlIcon ];
		filter: GTFilterSubstring