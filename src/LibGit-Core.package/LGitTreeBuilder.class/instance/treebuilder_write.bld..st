libgit-calls
treebuilder_write: id bld: bld
	
	^ self
		call: #(LGitReturnCodeEnum git_treebuilder_write #(LGitId * id , self))
		options: #()