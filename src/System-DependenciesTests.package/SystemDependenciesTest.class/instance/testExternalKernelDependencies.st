tests
testExternalKernelDependencies

	| dependencies | 
	
	self longTestCase.
	
	dependencies := self externalDependendiesOf: BaselineOfPharoBootstrap kernelPackageNames.
	
	self assertCollection: dependencies equals: self knownKernelDependencies