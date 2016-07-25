computing - dependencies
computeImageDependencies
	| visited |
	self newReport.
	report 
		analysisRunString: 'Pharo image dependencies';
		imageVersion: SystemVersion current.
	visited := Set new.
	RPackageOrganizer default packageNames 
		do: [ :packageName |
			self computeDependenciesOf: packageName visitedPackages: visited ].
	^ report