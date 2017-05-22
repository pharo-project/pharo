libgit-calls
tree_entry_free: handle
	
	^ self
		call: #(void git_tree_entry_free #(void *handle))
		options: #()