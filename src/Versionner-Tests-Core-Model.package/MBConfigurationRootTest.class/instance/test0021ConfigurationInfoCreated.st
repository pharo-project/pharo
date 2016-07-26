tests
test0021ConfigurationInfoCreated
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable |
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
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].
