libgit-calls
treebuilder_insert: out bld: bld filename: filename id: id filemode: filemode
	
	^ self
		call:
			#(LGitReturnCodeEnum git_treebuilder_insert #(LGitTreeEntry * out , self , String filename , LGitId * id , LGitFilemodeEnum filemode))
		options: #()