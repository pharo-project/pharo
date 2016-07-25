mc related utils
addRepositoryToPackageNamed: aString
	
	|pa|
	pa := MCPackage named: aString.
	pa workingCopy repositoryGroup addRepository: self repository.
	