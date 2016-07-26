build items
buildEdges
	packageCycle packages
		do: [ :package | 
			(packageCycle dependentPackagesFrom: package)
				do: [ :dependentPackage | graphviz add: package packageName -> dependentPackage packageName with: { #label -> (package dependenciesSizeTo: dependentPackage) } ] ]
		displayingProgress: 'Building edges'