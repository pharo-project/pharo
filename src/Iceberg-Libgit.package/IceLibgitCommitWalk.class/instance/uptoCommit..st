walk definition
uptoCommit: commit
	self revwalk hideCommit: (LGitId fromHexString: commit id)