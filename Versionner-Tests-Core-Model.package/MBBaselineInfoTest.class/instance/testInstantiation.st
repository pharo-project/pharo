tests
testInstantiation
	|  configurationInfo baseline |

	self should: [ MBBaselineInfo new ] raise: Error.
	self 
		shouldnt: [	configurationInfo := MBConfigurationInfo configurationClass: configuration.
	baseline := configurationInfo baselines detect: [:inf | inf version versionString = '1.0-baseline'].
] 
		raise: Error.
	
	self assert: (baseline versionString = '1.0-baseline').
	self assert: (info configurationInfo configurationClass == (Smalltalk at: #ConfigurationOfDummyConfiguration))