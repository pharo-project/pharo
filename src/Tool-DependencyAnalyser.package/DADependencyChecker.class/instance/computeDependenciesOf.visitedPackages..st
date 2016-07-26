private
computeDependenciesOf: aPackageName visitedPackages: visited
	| packageName |
	packageName := aPackageName asSymbol.
	report 
			add: packageName 
			dependencies: (self computeDependenciesOf: packageName).
	visited add: packageName