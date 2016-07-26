tests
test0082ConfigurationInfoChanged

	| configInfoDeleted configInfoChanged configInfoCreated createdValuable changedValuable deletedValuable configClassName configClass configInfo |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load version 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		self assert: configInfo currentVersion versionString = '1.2'.
		configInfo packages. "prime the pump"

		"modify class in MBFooTests project"
		(Smalltalk at: #MBFooTestA)
			compile: 'bar ^self' classified: 'mod'.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].
