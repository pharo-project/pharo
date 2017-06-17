libgit-calls
patch_from_diff: out diff: diff idx: idx
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_patch_from_diff #(LGitPatch * self , LGitDiff diff , FFIUInt32 idx))
		options: #()