tests
testInitialization
	| configClass conf |
	self 
		should: [ MBConfigurationInfo new ]
		raise: Error.
	configClass := Smalltalk globals at: #ConfigurationOfVersionner.
	self
		shouldnt: [ conf := MBConfigurationInfo configurationClass: configClass ]
		raise: Error.
	self 
		assert: conf name 
		equals: 'ConfigurationOfVersionner'