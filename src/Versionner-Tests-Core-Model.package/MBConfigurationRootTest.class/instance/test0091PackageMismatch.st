tests
test0091PackageMismatch
	"
	 	- load configuration dkh.3
		- load version 1.2
		- validate package printString
		- load MBFooTests-dkh.4
		- validate package printString
	"
	| configClass configClassName configInfo expected infos revertGofer |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	configClass := Smalltalk at: configClassName ifAbsent: [].
	configInfo := configurationRoot configurationInfoFor: configClass.

	"load version 1.1"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: (configInfo printString) = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.

	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isString].

	"load MBFooTests-dkh.4"
	gofer version: 'MBFooTests-dkh.4'.
	gofer load.

	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3[dkh.4]').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isText].
