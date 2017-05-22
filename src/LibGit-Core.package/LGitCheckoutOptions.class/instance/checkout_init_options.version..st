libgit-calls
checkout_init_options: opts version: version 
	^ self
		callUnchecked: #(int git_checkout_init_options(LGitCheckoutOptions * self, LGitOptionsVersionsEnum version))
		options: #(  )