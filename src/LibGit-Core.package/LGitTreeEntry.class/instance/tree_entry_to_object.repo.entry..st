libgit-calls
tree_entry_to_object: object_out repo: repo entry: entry
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tree_entry_to_object #(LGitObject * object_out , LGitRepository repo , self))
		options: #()