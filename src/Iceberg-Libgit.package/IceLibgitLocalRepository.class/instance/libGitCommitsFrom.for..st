private
libGitCommitsFrom: aLGitRepository for: aListOfCommitish
	^ aListOfCommitish collect: 
		[ :commitish | 
		LGitCommit of: aLGitRepository fromId: (LGitId fromHexString: commitish id) ]