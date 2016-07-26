tests
test0101ConfigurationInfoDeleted
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable configClass configInfo |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.
		configClassName := #ConfigurationOfMBFooTests.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		configClassName := #ConfigurationOfMBFooTests.
		self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
		configInfo := configurationRoot configurationInfoFor: configClass.
		self assert: configInfo printString = 'MBFooTests(dkh.1)'.

		"unload dkh.3"
		gofer unload.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self assert: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.
		self assert: (configurationRoot configurations includes: configInfo) not.

	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].
