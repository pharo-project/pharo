libgit-calls
tree_entry_id: entry
	
	^ self call: #(void * git_tree_entry_id #(self)) options: #()