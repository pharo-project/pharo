tests
testPackages
	| aCollection |
	self assert: aPackageRelationGraph packages isEmpty.
	aCollection := Bag new.
	aCollection add: packagePackageDependencies.
	aCollection add: packageKernel.
	aCollection do: [ :each | aPackageRelationGraph addPackage: each].
	self assert: (aPackageRelationGraph packages size = 2)