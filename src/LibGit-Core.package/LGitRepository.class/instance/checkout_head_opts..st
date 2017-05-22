libgit-calls
checkout_head_opts: opts
	^ self
		call:
			#(LGitReturnCodeEnum git_checkout_head #(self , LGitCheckoutOptions * opts))
		options: #()