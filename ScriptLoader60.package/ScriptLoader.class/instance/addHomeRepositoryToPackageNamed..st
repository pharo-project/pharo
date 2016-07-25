mc related utils
addHomeRepositoryToPackageNamed: aString
	
	|pa|
	pa := MCPackage named: aString.
	pa workingCopy repositoryGroup addRepository: self repository.
	