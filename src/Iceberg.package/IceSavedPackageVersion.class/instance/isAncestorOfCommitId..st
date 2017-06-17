comparing
isAncestorOfCommitId: anotherCommitId
	^ self commit isAncestorOf: (self repository commitAt: anotherCommitId)
