lookup
lookupBranch: name
	^ (name includes: $/)
		ifTrue: [ self lookupRemoteBranch: (name withoutPrefix: 'refs/remotes/') ]
		ifFalse: [ self lookupLocalBranch: name ] 
