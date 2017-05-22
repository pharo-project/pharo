commits
lastCommit
	"The first commit in the list was the last in time."
	^ self repository newCommitWalk fromBranch: self; firstCommit