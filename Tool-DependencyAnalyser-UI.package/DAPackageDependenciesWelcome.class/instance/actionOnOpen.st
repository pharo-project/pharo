protocol
actionOnOpen
	buttonOpen
		action: [ 
			self itemsFromPackageChosenList
				ifNotEmpty: [ 
					(DAPackageAnalyzerPackageDependenciesWindow onPackagesNamed: self itemsFromPackageChosenList) open.
					 ] ]