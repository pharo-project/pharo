libgit-calls
tree_entrycount: tree
	
	^ self call: #(int git_tree_entrycount #(self)) options: #()