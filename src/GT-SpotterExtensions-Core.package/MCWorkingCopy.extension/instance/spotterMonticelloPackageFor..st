*GT-SpotterExtensions-Core
spotterMonticelloPackageFor: aStep
	<spotterOrder: 21>
	aStep listProcessor
		title: 'Monticello Package';
		allCandidates: [ Array with: self package ];
		itemIcon: [ Smalltalk ui icons iconNamed: #monticelloPackageIcon ];
		filter: GTFilterSubstring