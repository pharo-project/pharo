tests
testPackageAt

	aPackageRelationGraph addPackage: packageKernel.
	self assert: ( (aPackageRelationGraph packageAt: 'Kernel') isNotNil).