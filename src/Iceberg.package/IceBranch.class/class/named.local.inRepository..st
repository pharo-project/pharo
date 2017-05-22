instance creation
named: branchName local: isLocal inRepository: repo
	^ (isLocal ifTrue: IceLocalBranch ifFalse: IceRemoteBranch)
		named: branchName inRepository: repo.