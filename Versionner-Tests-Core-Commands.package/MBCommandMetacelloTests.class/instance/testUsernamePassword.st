tests
testUsernamePassword
	
	| cmd configInfo |
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	self tearDownPackages add: 'ConfigurationOfMBFooTests'.
	configInfo := (MBConfigurationInfo configurationClass: (Smalltalk at: #'ConfigurationOfMBFooTests'))
		configurationRoot: MBConfigurationRoot new.
	cmd := MBSaveConfigurationCommand target: configInfo for: self.
	[cmd username] valueSupplyingAnswer: 'ab'.
	[cmd password] valueSupplyingAnswer: 'foobar'.
	
	self assert: cmd username = 'ab'.
	self assert: cmd password = 'foobar'.