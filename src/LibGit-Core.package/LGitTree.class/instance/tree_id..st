libgit-calls
tree_id: tree
	
	^ self call: #(void * git_tree_id #(self)) options: #()