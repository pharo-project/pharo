tests
test0080BasicVersionLoad
	"Eighth test:
	 	- load configuration dkh.3
		- load version 1.2
		- validate package and configInfo state
		- edit class and validate"
	| configClass configClassName configInfo expected infos x |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.
	self assert: configInfo currentVersion isNil.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isString.
	"versions"
	infos := configInfo versions.
	self assert: infos size = 3.
	expected := #(' @ 1.2' '1.1' '1.0 #stable').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isString ].
	
	"modify class in MBFooTests project"
	(Smalltalk at: #MBFooTestA)
		compile: 'bar ^self' classified: 'mod'.

	self assert: (x := configInfo printString) = '* MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isText.
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('* MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isText].
