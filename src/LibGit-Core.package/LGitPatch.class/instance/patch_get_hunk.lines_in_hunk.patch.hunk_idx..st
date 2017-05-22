libgit-calls
patch_get_hunk: out lines_in_hunk: lines_in_hunk patch: patch hunk_idx: hunk_idx
	
	^ self
		call:
			#(LGitReturnCodeEnum git_patch_get_hunk #(LGitDiffHunk ** out , LGitWriteBackValueUInt * lines_in_hunk , LGitPatch * patch , FFIUInt32 hunk_idx))
		options: #()