libgit-calls
reference_cmp: ref1 ref2: ref2
	
	^ self
		call: #(LGitReturnCodeEnum git_reference_cmp #(self , LGitReference ref2))
		options: #()