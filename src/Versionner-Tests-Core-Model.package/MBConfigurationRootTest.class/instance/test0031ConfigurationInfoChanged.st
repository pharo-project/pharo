tests
test0031ConfigurationInfoChanged
	"If you don't touch the currentVersion of a configurationInfo, then no changed event is signaled on load."
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.

		"load configuration"
		configClassName := #ConfigurationOfMBFooTests.
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].
