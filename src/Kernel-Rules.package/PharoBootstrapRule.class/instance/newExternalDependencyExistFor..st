instance creation
newExternalDependencyExistFor: aPackage
	"Check if there are new dependencies to a package outside this layer"
	| dependencies newDependencies |
	dependencies := self dependencyChecker dependenciesOf: aPackage packageName.
	newDependencies := dependencies difference: self bootstrapPackages.
	^ newDependencies notEmpty