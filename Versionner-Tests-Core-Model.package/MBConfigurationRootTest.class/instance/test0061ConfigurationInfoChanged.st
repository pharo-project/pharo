tests
test0061ConfigurationInfoChanged
	"Touch the currentVersion of a configurationInfo, expect a changed event on load"
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable configInfo configClass |
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
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		self assert: configInfo currentVersion versionString = '1.0'.
		configInfo packages. "prime the pump"

		"load 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2') load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].
