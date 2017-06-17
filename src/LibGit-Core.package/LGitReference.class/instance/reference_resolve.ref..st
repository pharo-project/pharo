libgit-calls
reference_resolve: out ref: ref
	
	^ self
		callUnchecked: #(LGitReturnCodeEnum git_reference_resolve #(LGitReference * out , self))
		options: #()