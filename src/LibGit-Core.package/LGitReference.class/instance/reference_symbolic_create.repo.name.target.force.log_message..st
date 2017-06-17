libgit-calls
reference_symbolic_create: out repo: repo name: name target: target force: force log_message: log_message
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_symbolic_create #(LGitReference * self , LGitRepository repo , String name , String target , Boolean force , String log_message))
		options: #(optCoerceNilToNull)