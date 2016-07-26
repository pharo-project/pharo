tests
test0070BasicVersionLoad
	"Seventh test:
	 	- load configuration dkh.3
		- load version 1.0.
		- validate package, baselines and versions state
		- load version 1.2
		- validate new package, baselines and versions state"
	| configClass configClassName configInfo expected infos |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.
	self assert: configInfo currentVersion isNil.

	"load version 1.0"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.0'.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.3)'.
	"baselines"
	infos := configInfo baselines.
	self assert: infos size = 1.
	expected := #('1.0-baseline #bleedingEdge').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"versions"
	infos := configInfo versions.
	self assert: infos size = 3.
	expected := #('1.2' '1.1' ' @ 1.0 #stable').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.1').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	"baselines"
	infos := configInfo baselines.
	self assert: infos size = 1.
	expected := #('1.0-baseline #bleedingEdge').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
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
		self assert: info printString = (expected at: index)].
