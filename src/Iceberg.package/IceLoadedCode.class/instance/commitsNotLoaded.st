querying
commitsNotLoaded
	^ self repository newCommitWalk
		fromHead;
		upto: self referenceCommit;
		commits 
