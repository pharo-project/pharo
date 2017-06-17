libgit-calls
checkout_treeish: treeish
	"treeish can be a commit, tag or tree (or NULL to use HEAD)"
	self withReturnHandlerDo: [ 
		self
			checkout_treeish: treeish
			opts:
				(LGitCheckoutOptions defaults
					checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_safe;
					yourself) ].
