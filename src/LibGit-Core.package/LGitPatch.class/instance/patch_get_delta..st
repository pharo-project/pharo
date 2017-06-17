libgit-calls
patch_get_delta: patch
	
	^ self
		call: #(LGitDiffDelta * git_patch_get_delta #(self))
		options: #()