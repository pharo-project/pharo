querying
versionFor: aCommitId
	^ (self repository commitAt: aCommitId) versionFor: self