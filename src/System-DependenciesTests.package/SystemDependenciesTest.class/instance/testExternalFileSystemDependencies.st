tests
testExternalFileSystemDependencies

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames,
		BaselineOfPharoBootstrap fileSystemPackageNames).
	
	self assertCollection: dependencies equals: self knownFileSystemDependencies.