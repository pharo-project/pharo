commits
outgoingCommitsTo: aRemote do: aBlock
	| walk | 
	walk := self repository newCommitWalk fromBranch: self.
	(self upstreamFor: aRemote)
		ifNotNil: [ :aRemoteBranch | walk uptoBranch: aRemoteBranch ].

	aBlock cull: walk.
	^ walk commits.