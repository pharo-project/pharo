libgit-calls
tree_entry_byname: tree name: name
	
	^ self
		call: #(LGitTreeEntry git_tree_entry_byname #(self , String name))
		options: #()