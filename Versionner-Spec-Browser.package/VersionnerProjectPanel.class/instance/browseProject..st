actions
browseProject: aConfigurationClass
	| configInfo |
	configInfo := MBConfigurationRoot current configurationInfoFor: aConfigurationClass.
	configInfo ifNotNil: 
		[ configInfo recalculate.
		^ VersionnerProjectBrowser openOn: configInfo ]