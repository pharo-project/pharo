api
configuration: aMBConfigurationInfo 
	configuration := aMBConfigurationInfo.
	configuration ifNotNil: [ 
		configuration recalculateSilently.
		self versionList: configuration versions ].