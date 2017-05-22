querying
outgoingCommits
	^ self repository currentBranch outgoingCommits: [ :walk | walk modifyingPackage: self ]
