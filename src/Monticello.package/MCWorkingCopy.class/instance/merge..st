operations
merge: targetVersion
	| ancestorInfo merger ancestorSnapshot packageSnapshot |
	targetVersion dependencies do: [:ea | ea resolve merge].
	ancestorInfo := targetVersion info commonAncestorWith: ancestry.
	
	ancestorInfo = targetVersion info ifTrue: [^ MCNoChangesException signal].
	
	packageSnapshot := package snapshot.
	ancestorSnapshot := ancestorInfo
							ifNotNil: [(self findSnapshotWithVersionInfo: ancestorInfo)]
							ifNil: [self notifyNoCommonAncestorWith: targetVersion.  MCSnapshot empty].
	
	(ancestry ancestors size = 1
		and: [ancestry ancestors first = ancestorInfo
				and: [(packageSnapshot patchRelativeToBase: ancestorSnapshot) isEmpty]])
				ifTrue: [^ targetVersion load].
	
	merger := MCThreeWayMerger 
				base: packageSnapshot
				target: targetVersion snapshot
				ancestor: ancestorSnapshot.
	((MCMergeResolutionRequest new merger: merger)
		signal: 'Merging ', targetVersion info name) = true 
			ifTrue:[
				merger loadWithNameLike: targetVersion info name.
				ancestry addAncestor: targetVersion info ].
	self changed