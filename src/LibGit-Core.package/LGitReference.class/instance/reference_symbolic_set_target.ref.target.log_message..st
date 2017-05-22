libgit-calls
reference_symbolic_set_target: out ref: ref target: target log_message: log_message
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_symbolic_set_target #(LGitReference * self , LGitReference ref , String target , String log_message))
		options: #()