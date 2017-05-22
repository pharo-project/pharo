comparing
isDescendantOf: aCommit
	^ self repository newCommitWalk
		fromCommit: self;
		includesCommit: aCommit