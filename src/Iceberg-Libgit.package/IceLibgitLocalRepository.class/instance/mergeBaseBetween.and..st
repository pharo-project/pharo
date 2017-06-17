querying
mergeBaseBetween: aCommitId and: anotherCommitId 
	self withRepoDo: [ :repo |
		^ (repo 
				mergeBaseBetween: (LGitId fromHexString: aCommitId) 
				and: (LGitId fromHexString: anotherCommitId)) 
				hexString ]