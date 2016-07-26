computing - dependencies
transitiveDependenciesOf: aPackageName
	self newReport.
	report 
		analysisRunString: 'Transitive dependencies of ' , aPackageName;
		imageVersion: SystemVersion current.
	self computeTransitiveDependenciesOf: aPackageName visitedPackages: Set new.
	^ report