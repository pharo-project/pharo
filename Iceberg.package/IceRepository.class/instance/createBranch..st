actions
createBranch: branchName
	self command: {'checkout'. '-b'. branchName}.
	self branch: branchName.
	self refresh.
