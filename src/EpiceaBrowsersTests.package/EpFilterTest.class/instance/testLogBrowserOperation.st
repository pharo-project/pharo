tests
testLogBrowserOperation
	self
		assert: EpPluggableFilter logBrowserOperationFilter
		accepts: self redo
		rejects: self methodAddition