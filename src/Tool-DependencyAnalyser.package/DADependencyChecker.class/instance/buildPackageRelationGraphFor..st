private
buildPackageRelationGraphFor: aPackageName
	^ [ (DAPackageRelationGraph onPackagesNamed: { aPackageName }) build ]
		on: PotentialOutDatedDependencyWarning 
		do: [ :ex | report addWarning: ex. ex resume ]