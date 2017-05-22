libgit-calls
reference_create: out repo: repo name: name id: id force: force log_message: log_message
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_reference_create #(LGitReference * self , LGitRepository repo , String name , LGitId * id , Boolean force , String log_message))
		options: #(optCoerceNilToNull)