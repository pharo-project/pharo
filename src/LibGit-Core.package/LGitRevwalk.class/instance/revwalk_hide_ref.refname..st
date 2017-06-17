libgit-calls
revwalk_hide_ref: walk refname: refname
	
	^ self
		call:
			#(LGitReturnCodeEnum git_revwalk_hide_ref #(self , FFIExternalString refname))
		options: #()