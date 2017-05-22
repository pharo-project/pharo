operations
checkout
	self withReturnHandlerDo: [ 
		self checkout_index_withOptions: 
			(LGitCheckoutOptions defaults
				checkoutStrategy: LGitCheckoutStrategyEnum git_checkout_force;
				yourself) ]