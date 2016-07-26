protocol
actionOnFindCycles
	| selectedPackages selectedItems |
	selectedItems := self selectedItemsFromTree.
	(self arePackageNodes: selectedItems)
		ifTrue: [ 
			self notify: 'This could take a while, confirm?'.
			selectedPackages := self pdPackagesFrom: selectedItems.
			systemCycles ifNil: [ systemCycles := DAPackageAnalyzerCycleDetectionWindow  system ].
			systemCycles
				updateWithFilter: selectedPackages;
				open ]