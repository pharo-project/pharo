libgit-calls
patch_get_line_in_hunk: out patch: patch hunk_idx: hunk_idx line_of_hunk: line_of_hunk
	
	^ self
		call:
			#(LGitReturnCodeEnum git_patch_get_line_in_hunk #(LGitDiffLine * out , self , FFIUInt32 hunk_idx , FFIUInt32 line_of_hunk))
		options: #()