libgit-calls
tree_entry_filemode: entry
	
	^ self
		call: #(LGitFilemodeEnum git_tree_entry_filemode #(self))
		options: #()