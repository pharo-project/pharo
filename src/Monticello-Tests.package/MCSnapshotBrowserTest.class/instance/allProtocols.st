private
allProtocols
	^ MCSnapshotResource current definitions
		select: [:def | def isMethodDefinition]
		thenCollect: [:def | def category]		