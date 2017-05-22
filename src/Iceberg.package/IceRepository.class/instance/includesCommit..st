commits
includesCommit: anIceCommit
	^ [ self commitAt: anIceCommit id. true ] 
		on: LGit_GIT_ENOTFOUND do: [ false ]