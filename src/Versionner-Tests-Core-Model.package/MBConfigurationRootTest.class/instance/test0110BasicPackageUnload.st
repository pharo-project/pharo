tests
test0110BasicPackageUnload

	"load dkh.3"
	| configClassName configClass configInfo unloadGofer infos |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.

	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.

	"unload MBFooTests-dkh.3"
	unloadGofer := Gofer new.
	unloadGofer disablePackageCache.
	unloadGofer version: 'MBFooTests-dkh.3'.
	[ unloadGofer unload ] on: Warning do: [:ex | ex resume ].

	self assert: (configInfo printString) = 'MBFooTests(dkh.3)'.
	self assert: configInfo asStringOrText isString.
	"packages"
	infos := configInfo packages.
	self assert: infos size = 0.
