initialization
initializeWithPackageNames: aCollection
	packageAnalyzerTreeModel := DAPackageAnalyzerTreeModel onPackagesNamed: aCollection.
	self buildandAddAllMorph 