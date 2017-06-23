querying
parent
	^ (self repository newCommitWalk 
		fromCommit: self commit; 
		modifyingPackage: self package;
		maxNumber: 2;
		commits)
		second "First commit will be my own commit, the second is my parent"
		versionFor: self package.