lookup
lookupLocalBranch: name ifAbsent: aBlock
	^ [ self lookupLocalBranch: name ]
		on: LGit_GIT_ENOTFOUND 
		do: aBlock 