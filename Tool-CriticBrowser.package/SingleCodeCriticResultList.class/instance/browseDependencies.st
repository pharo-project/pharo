menu
browseDependencies
	criticsModel selectedItems ifNotNil: [ :packages |
		(DAPackageAnalyzerPackageDependenciesWindow onPackagesNamed: (packages collect: #packageName)) open ]