querying
changesIn: commitish comparedToBase: baseCommitish
	^ (MCPatch 
		fromBase: (baseCommitish snapshotFor: self)
		target: (commitish snapshotFor: self))
		operations