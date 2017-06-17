operations
diffTree: oldTree toTree: newTree
	self
		diffTree: oldTree
		toTree: newTree
		options: LGitDiffOptions defaults