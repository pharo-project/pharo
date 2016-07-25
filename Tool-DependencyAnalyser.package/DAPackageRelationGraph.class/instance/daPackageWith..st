accessing
daPackageWith: packageName
	^ DAPackage on: (RPackageSet named: packageName)