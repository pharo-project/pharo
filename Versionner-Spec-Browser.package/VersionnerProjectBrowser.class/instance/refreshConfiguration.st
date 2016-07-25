refreshing
refreshConfiguration
	configuration
		ifNotNil: [ self configuration: (MBConfigurationRoot current configurationInfoFor: configuration configurationClass) ]