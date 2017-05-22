accessing
upstreamForBranch: aBranch backend: backend
	^ backend 
		lookupBranchNamed: aBranch name 
		inRemote: self remoteName
