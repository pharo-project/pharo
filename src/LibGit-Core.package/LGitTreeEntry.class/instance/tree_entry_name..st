libgit-calls
tree_entry_name: entry
	
	^ self call: #(String git_tree_entry_name #(self)) options: #()