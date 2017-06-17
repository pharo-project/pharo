libgit-calls
index_remove_all: index pathspec: pathspec callback: callback payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_index_remove_all #(self , LGitStringArray * pathspec , LGitIndexMatchedPathCallback callback , FFIVoid * payload))
		options: #(optMayGC optCoerceNilToNull)