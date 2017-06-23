private
lookupCommit: commitId
	^ self backend newCommitWalk 
		fromCommitId: commitId; 
		firstCommit
