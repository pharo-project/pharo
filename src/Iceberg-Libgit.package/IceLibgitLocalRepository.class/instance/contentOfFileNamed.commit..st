querying
contentOfFileNamed: aPathString commit: aCommitId
	self withRepoDo: [ :repo |
		| commit |
		commit := LGitCommit of: repo fromHexString: aCommitId.
		^ (commit entryByPath: aPathString) object contents
	]