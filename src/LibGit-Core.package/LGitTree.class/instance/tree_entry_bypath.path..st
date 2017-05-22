libgit-calls
tree_entry_bypath: out path: path
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tree_entry_bypath #(LGitTreeEntry * out , self , String path))
		options: #()