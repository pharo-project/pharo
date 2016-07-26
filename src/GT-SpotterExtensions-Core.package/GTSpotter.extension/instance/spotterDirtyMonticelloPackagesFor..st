*GT-SpotterExtensions-Core
spotterDirtyMonticelloPackagesFor: aStep
	<spotterOrder: 85>
	aStep listProcessor
			allCandidates: [ MCWorkingCopy allManagers select: #modified ];
			title: 'Dirty Monticello packages';
			itemIcon: [ Smalltalk ui icons iconNamed: #dirtyPackageIcon ];
			filter: GTFilterSubstring;
			wantsToDisplayOnEmptyQuery: true