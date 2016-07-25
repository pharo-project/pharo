tests
test0032InfoListChanged

	| infoListChanged changedValuable |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].
