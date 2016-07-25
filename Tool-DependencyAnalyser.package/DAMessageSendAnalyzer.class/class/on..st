instance creation
on: aPackageName
	| packageAnalysis |
	packageAnalysis := ((DAPackageRelationGraph onPackagesNamed: { aPackageName })
				build;
				packages)
			select: #isSeen.
	^ self basicNew
		initializeWith: packageAnalysis first;
		yourself