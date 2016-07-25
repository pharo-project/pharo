packages
categoriesInPackageNamed: packageName
	^(self environment organization categoriesMatching: packageName), (self environment organization categoriesMatching: (packageName, '-*'))