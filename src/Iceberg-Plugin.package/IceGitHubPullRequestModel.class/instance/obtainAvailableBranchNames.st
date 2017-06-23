private
obtainAvailableBranchNames
	^ ((self repository allBranches 
		collect: #basename as: Set)
		addAll: self baseBranchNames;
		asArray)
		sort.
	