accessing
transitiveDependenciesOf: aPackageName withoutDependenciesOf: aListOfPackageNames
	"Do not add noise to the analysis by adding transitive dependencies of dependencies we want to remove."
	^ self 
		transitiveDependenciesOf: aPackageName
		visited: (aListOfPackageNames collect: #asSymbol as: Set)