libgit-calls
packbuilder_insert_tree: pb id: id
	
	^ self
		call: #(LGitReturnCodeEnum git_packbuilder_insert_tree #(self , LGitId * id))
		options: #(optMayGC)