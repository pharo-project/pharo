initialization
initializeWithRPackageSet: aCollection
	relationGraph := DAPackageRelationGraph onPackages: (aCollection collect: [ :each | DAPackage on: each ]).
	cycles := OrderedCollection new.
	self relationGraph build.
	sccs := DATarjanAlgorithm new
		run: self relationGraph;
		stronglyConnectedComponents