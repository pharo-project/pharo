tests
testClearAllDependencies
	| aRelationGraph packagesFromGraph |
	aRelationGraph := DAPackageRelationGraph
		onPackages:
			(#('Collections-Abstract' 'Collections-Sequenceable') collect: [ :each | DAPackage on: (RPackageSet named: each) ]).
	aRelationGraph
		build;
		clearAllDependencies.
	packagesFromGraph := aRelationGraph seenPackages.
	self assert: (packagesFromGraph anySatisfy: [ :package | package dependencies isEmpty ])