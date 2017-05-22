libgit-calls
signature_default: out repo: repo
	
	^ self
		call:
			#(LGitReturnCodeEnum git_signature_default #(ExternalAddress ** out , self))
		options: #()