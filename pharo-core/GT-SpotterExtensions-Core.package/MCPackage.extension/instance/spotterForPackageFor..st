*GT-SpotterExtensions-Core
spotterForPackageFor: aStep
	<spotterOrder: 2>
	aStep listProcessor
		title: 'Packages';
		allCandidates: [ Array with: self correspondingRPackage ];
		itemName: [ :package | package name ];
		itemIcon: [ Smalltalk ui icons iconNamed: #packageIcon ];
		filter: GTFilterSubstring