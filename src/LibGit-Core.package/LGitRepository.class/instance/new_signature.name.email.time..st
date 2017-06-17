libgit-calls
new_signature: out name:name email:email time:time
	
	^ self
		call:
			#(LGitReturnCodeEnum git_signature_new #(ExternalAddress ** out , String name, String email, LGitTime time, 0))
		options: #()