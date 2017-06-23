querying
branchesForMerge: aCommit
	"Returns a list of local branches to which we could merge the received commit"
	^ self allBranches
		select: [:each | (each includesCommit: aCommit) not ].
