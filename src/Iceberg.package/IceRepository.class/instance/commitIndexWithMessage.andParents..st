actions
commitIndexWithMessage: message andParents: parentCommitishList
	| packages newCommit |
	packages := self stagedPackages.
	newCommit := self backend
		commitIndexWithMessage: message andParents: parentCommitishList.
	self loadedCode 
		commited: newCommit changingPackages: packages.
	^ newCommit