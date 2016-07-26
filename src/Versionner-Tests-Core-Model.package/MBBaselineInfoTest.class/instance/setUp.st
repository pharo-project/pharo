running
setUp
	| configurationInfo |
	super setUp.
	configurationInfo := MBConfigurationInfo configurationClass: configuration.
	info := configurationInfo baselines detect: [:inf | inf version versionString = '1.0-baseline']
