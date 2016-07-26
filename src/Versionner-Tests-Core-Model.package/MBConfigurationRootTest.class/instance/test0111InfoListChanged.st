tests
test0111InfoListChanged
	| infoListChanged changedValuable configClassName configClass configInfo unloadGofer |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load version 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.

		self deny: infoListChanged.
		infoListChanged := false.

		self assert: configInfo currentVersion versionString = '1.2'.
		configInfo infoList. "prime the pump"

		"unload MBFooTests-dkh.3"
		unloadGofer := Gofer new.
		unloadGofer disablePackageCache.
		unloadGofer version: 'MBFooTests-dkh.3'.
		[ unloadGofer unload ] on: Warning do: [:ex | ex resume ].

		self assert: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].

