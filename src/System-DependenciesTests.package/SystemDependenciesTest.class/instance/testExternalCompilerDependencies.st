tests
testExternalCompilerDependencies

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: (
		BaselineOfPharoBootstrap kernelPackageNames,
		BaselineOfPharoBootstrap compilerPackageNames).
	
	self assertCollection: dependencies equals: self knownCompilerDependencies.