libgit-calls
checkout_index_withOptions: opts
	^ self
		call: #(LGitReturnCodeEnum 
			git_checkout_index(
				LGitRepository repository, 
				self, 
				LGitCheckoutOptions *opts))
		options: #()
