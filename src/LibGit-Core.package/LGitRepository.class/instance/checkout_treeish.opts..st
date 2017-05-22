libgit-calls
checkout_treeish: treeish opts: opts
	^ self
		call:
			#(LGitReturnCodeEnum git_checkout_tree #(self , LGitObject treeish , LGitCheckoutOptions * opts))
		options: #()