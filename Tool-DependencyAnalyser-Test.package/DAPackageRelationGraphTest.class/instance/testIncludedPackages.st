tests
testIncludedPackages
	| aCollection |
	aCollection := Bag new.
	"we add 2 packages in the relation"
	aCollection add: packagePackageDependencies .
	aCollection add: packageKernel.
	aCollection do: [ :each | aPackageRelationGraph addPackage: each].
	self assert: (aPackageRelationGraph includedPackages  size = 2).