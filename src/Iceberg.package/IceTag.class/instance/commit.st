querying
commit
	^ commit ifNil: [ commit := self repository newCommitWalk fromTag: self; firstCommit ]