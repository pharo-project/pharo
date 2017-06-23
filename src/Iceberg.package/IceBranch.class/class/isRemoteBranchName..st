testing
isRemoteBranchName: branchName
	^ ((branchName beginsWith: 'refs/remotes/') 
		or: [ branchName beginsWith: 'remotes/' ])