operations
checkoutHead
	self withReturnHandlerDo: [ 
		self checkout_head_opts: (
			LGitCheckoutOptions defaults
				checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_safe;
				yourself) ]
