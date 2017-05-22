private
timeSinceLastCommit: json
	| branchName branchJson lastCommitDate |
	branchName := json at: 'name'.
	branchJson := branchesByName at: branchName.
	lastCommitDate := ((((branchJson at: 'commit') at: 'commit') at: 'author') at: 'date') asDateAndTime.
	^ '{1} days' format: { (Date today - lastCommitDate) days }