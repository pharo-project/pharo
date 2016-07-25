accessing
loadRemoteDependenciesIn: aRepository
	"load the remote dependencies of all packages in parallel"
	| dependencies |
	dependencies := self 
		requiredPackagesParallelCollect: [ :aPackage|
			(aPackage workingCopy closestAncestorSnapshotIn: aRepository) definitions.] 
		withMessage: 'Loading remote dependencies...'.
	^ dependencies flattened
