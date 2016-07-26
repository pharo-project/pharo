private
definedClasses
	^ MCSnapshotResource current definitions 
		select: [:def | def isClassDefinition] 
		thenCollect: [:def | def className].