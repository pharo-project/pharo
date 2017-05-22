querying
lookupBranchNamed: branchName inRemote: remoteName
	self withRepoDo: [ :repo |
		[ repo lookupRemoteBranch: remoteName, '/', branchName ]
			on: LGit_GIT_ENOTFOUND do: [ ^ nil ].
		
		^ IceRemoteBranch 
			remoteName: remoteName
			branchName: branchName
			inRepository: self frontend
	]
