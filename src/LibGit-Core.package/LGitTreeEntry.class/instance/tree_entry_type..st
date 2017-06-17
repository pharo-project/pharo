libgit-calls
tree_entry_type: entry
	
	^ self
		call: #(LGitObjectTypeEnum git_tree_entry_type #(self))
		options: #()