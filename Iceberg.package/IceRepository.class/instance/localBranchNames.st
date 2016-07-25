querying
localBranchNames
	^ (self command: 'branch') lines collect: [ :line | line allButFirst: 2 ]