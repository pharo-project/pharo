accessing
dependentPackagesDiffRemoved
	^ dependentPackagesDiff select: [ :each | each isRemoved ]