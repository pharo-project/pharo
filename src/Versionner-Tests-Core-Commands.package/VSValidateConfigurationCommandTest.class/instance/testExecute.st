tests
testExecute
	self
		should: [ (VSValidateConfigurationCommand target: (MBConfigurationInfo configurationClass: configuration) for: nil) execute ]
		raise: MetacelloStackCacheNotification