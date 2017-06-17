operations
diffTree: oldTree toTree: newTree options: anLGitDiffOptions
	self withReturnHandlerDo: [
		self
			diff_tree_to_tree: self
			repo: self repository
			old_tree: oldTree
			new_tree: newTree
			opts: anLGitDiffOptions ]