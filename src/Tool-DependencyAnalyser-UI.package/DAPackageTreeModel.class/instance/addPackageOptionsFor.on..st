actions
addPackageOptionsFor: selectedNodes on: menuGroup
	| selectedPackageNames |
	(self arePackageNodes: selectedNodes)
		ifTrue: [ 
			selectedPackageNames := selectedNodes collect: [ :packageNode | packageNode packageName ].
			menuGroup
				addMenuItem:
					(MenuItemPresenter new
						name: 'Browse scoped dependencies';
						action: [ (DAPackageAnalyzerPackageDependenciesWindow onPackagesNamed: selectedPackageNames) open ]).
			selectedNodes size > 1
				ifTrue: [ 
					menuGroup
						addMenuItem:
							(MenuItemPresenter new
								name: 'Find cycles among packages';
								action: [ (DAPackageAnalyzerCycleDetectionWindow onPackagesNamed: selectedPackageNames) open ]) ] ]