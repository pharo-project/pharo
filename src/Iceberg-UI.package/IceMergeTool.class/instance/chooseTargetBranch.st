utilities
chooseTargetBranch
	| branches |
	
	self targetBranch ifNotNil: [ ^ self targetBranch ].
	
	branches := self repository branchesForMerge: self source.
	^ targetBranch := self uiManager
		chooseFrom: (branches collect: #name)
		values: branches
		message: ('Select the branch into you want to merge commit {1}' format: { source shortId })
		title: 'Merge your changes into another branch'.

