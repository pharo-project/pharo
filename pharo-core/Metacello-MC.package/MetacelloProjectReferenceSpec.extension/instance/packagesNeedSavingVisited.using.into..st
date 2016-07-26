*metacello-mc
packagesNeedSavingVisited: visitedProjects using: repos into: aCollection
	
	| prjct clsName vrsn |
	prjct := self resolveToLoadableSpec.
	(visitedProjects includes: (clsName := prjct className)) ifTrue: [ ^self ].
	visitedProjects add: clsName.
	(vrsn := self versionOrNil) == nil ifTrue: [ ^self ].
	vrsn spec 
		packagesNeedSavingVisited: visitedProjects 
		into: aCollection