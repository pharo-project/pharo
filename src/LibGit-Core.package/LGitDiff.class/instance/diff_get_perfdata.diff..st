libgit-calls
diff_get_perfdata: out diff: diff
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_get_perfdata #(LGitDiffPerfData * out , self))
		options: #()