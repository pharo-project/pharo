patches
mcVersionFor: iceVersion
	self withRepoDo: [ :repo | 
		| commit |
		commit := LGitCommit of: repo fromId: (LGitId fromHexString: iceVersion commit id).

		[ ^ (IceLibgitVersionReader 
				on: commit
				iceVersion: iceVersion) 
				version ] 
			on: IceMissingRepositoryEntry 
			do: [ IceVersionDoesNotExist new version: iceVersion; signal ]
	]
