lookup
lookupBranch: branchName inRemote: remoteName
	^ self lookupRemoteBranch: remoteName, '/', branchName