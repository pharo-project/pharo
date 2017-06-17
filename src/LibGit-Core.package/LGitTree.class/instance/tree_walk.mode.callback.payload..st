libgit-calls
tree_walk: tree mode: mode callback: callback payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_tree_walk #(LGitTree tree , LGitTreewalkModeEnum mode , LGitTreewalkCallback callback , FFIVoid * payload))
		options: #(optMayGC optCoerceNilToNull)