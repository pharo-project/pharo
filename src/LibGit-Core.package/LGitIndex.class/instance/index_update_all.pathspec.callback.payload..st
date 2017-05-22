libgit-calls
index_update_all: index pathspec: pathspec callback: callback payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_index_update_all #(self , LGitStringArray * pathspec , LGitIndexMatchedPathCallback callback , FFIVoid * payload))
		options: #(optMayGC optCoerceNilToNull)