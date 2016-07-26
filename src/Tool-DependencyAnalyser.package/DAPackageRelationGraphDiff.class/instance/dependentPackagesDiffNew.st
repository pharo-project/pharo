accessing
dependentPackagesDiffNew
	^ dependentPackagesDiff select: [ :each | each isNew ]