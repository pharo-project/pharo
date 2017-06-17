libgit-calls
reference_lookup: out repo: repo shorthand: shorthand
	
	^ self
		call:
			#(LGitReturnCodeEnum git_reference_lookup #(LGitReference * out , self , String shorthand))
		options: #()