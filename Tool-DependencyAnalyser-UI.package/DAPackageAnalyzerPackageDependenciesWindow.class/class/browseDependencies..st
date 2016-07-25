nautilus
browseDependencies: aBuilder
	<contextMenu>
	<nautilusGlobalPackageMenu>
	(aBuilder item: #'Browse dependencies...')
		action: [ (self onPackagesNamed: (self selectedPackagesFrom: aBuilder)) open ];
		order: 1000.