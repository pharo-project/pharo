libgit-calls
tag_peel: result
	^ self
		call: #(LGitReturnCodeEnum git_tag_peel #(LGitObject * result, LGitTag self))
		options: #()