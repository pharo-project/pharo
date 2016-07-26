tests
test0062InfoListChanged
	| infoListChanged changedValuable configClassName configClass configInfo |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		configClassName := #ConfigurationOfMBFooTests.
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: infoListChanged.
		infoListChanged := false.

		self assert: configInfo currentVersion versionString = '1.0'.
		configInfo infoList. "prime the pump"

		"load 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2') load.

		self assert: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].
