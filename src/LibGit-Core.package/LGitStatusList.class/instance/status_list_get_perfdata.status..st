libgit-calls
status_list_get_perfdata: out status: status
	
	^ self
		call:
			#(LGitReturnCodeEnum git_status_list_get_perfdata #(LGitPerfData * out , self))
		options: #()