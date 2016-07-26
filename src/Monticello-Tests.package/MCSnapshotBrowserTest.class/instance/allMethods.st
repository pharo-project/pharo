private
allMethods
	^ MCSnapshotResource current definitions
		select: [:def | def isMethodDefinition]
		thenCollect: [:def | def selector]		