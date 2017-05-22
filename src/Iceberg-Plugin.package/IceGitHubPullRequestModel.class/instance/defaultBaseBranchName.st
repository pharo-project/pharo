accessing
defaultBaseBranchName
	self hasParent ifFalse: [ ^ self baseDefaultBranchName ].
	^ self baseBranchNames 
		detect: [ :each | each = self currentBranch basename ] 
		ifNone: [ self baseDefaultBranchName ]