commits
commitsNotIn: anotherBranch
	^ self repository newCommitWalk
		fromBranch: self;
		uptoBranch: anotherBranch;
		commits
