system changes
bestMatchingManagerForCategory: aSystemCategory do: aBlock
	| bestMatches bestMatchingManagerAndPackage |
	bestMatches := OrderedCollection new.
	self registry do: [ :mgr |
		| candidatePackages bestMatchingPackage |
		candidatePackages := mgr packageSet packages select: [ :package |
			package name beginsWith: aSystemCategory ].
		bestMatchingPackage := candidatePackages detectMin: [ :package |
			package name size ].
		bestMatchingPackage ifNotNil: [
			bestMatches add: mgr -> bestMatchingPackage ] ].
	bestMatchingManagerAndPackage := bestMatches detectMin: [ :managerAndPackage |
			managerAndPackage value name size ].
	bestMatchingManagerAndPackage ifNotNil: [ :managerAndPackage |
		aBlock value: managerAndPackage key ]