libgit-calls
tree_owner: tree
	
	^ self call: #(LGitRepository git_tree_owner #(self)) options: #()