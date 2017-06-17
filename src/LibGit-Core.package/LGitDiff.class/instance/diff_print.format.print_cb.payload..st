libgit-calls
diff_print: diff format: format print_cb: print_cb payload: payload
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_print #(self , LGitDiffFormatTypeEnum format , LGitDiffLineCallback print_cb , FFIVoid * payload))
		options: #(optMayGC optCoerceNilToNull)