*GT-SpotterExtensions-Core
spotterForPackagesFor: aStep
	<spotterOrder: 20>
	aStep listProcessor
			allCandidates: [ RPackageOrganizer default packages ];
			title: 'Packages';
			itemName: [ :package | package name ];
			itemIcon: [ Smalltalk ui icons iconNamed: #packageIcon ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: false