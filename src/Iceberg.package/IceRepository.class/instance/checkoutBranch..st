actions
checkoutBranch: branchName
	self branch: branchName.
	self command: {'checkout'. branchName}.
	self refresh.
