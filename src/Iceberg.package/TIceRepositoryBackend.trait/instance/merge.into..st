actions
merge: aCommitId into: aBranchName
	| currentBranchName |
	currentBranchName := self branch name.
	[ 
		self checkoutBranch: aBranchName.
		self merge: aCommitId ]
	ensure: [ 
		self checkoutBranch: currentBranchName ]
