accessing
manuallyResolvedDependencies
	^ self packageUnderAnalysis manuallyResolvedDependencies
		collect: [ :packageName | DAManuallyAddedDependentPackageNode new content: packageName ]