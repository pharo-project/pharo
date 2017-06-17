instance creation
of: aRepository fromHexString: aCommitId
	^ self of: aRepository fromId: (LGitId fromHexString: aCommitId)