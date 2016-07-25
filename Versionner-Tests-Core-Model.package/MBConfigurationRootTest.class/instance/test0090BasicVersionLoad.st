tests
test0090BasicVersionLoad
	"9:
	 	- load configuration dkh.3
		- load version 1.2
		- edit class
		- verify state
		- revert MBFooTests-dkh.3
		- validate"
	| configClass configClassName configInfo expected infos revertGofer |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.
	self assert: configInfo currentVersion isNil.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: (configInfo printString) = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.

	"modify class in MBFooTests project"
	(Smalltalk at: #MBFooTestA)
		compile: 'bar ^self' classified: 'mod'.

	self assert: (configInfo printString) = '* MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isText.
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('* MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isText].

	"revert MBFooTests-dkh.3"
	revertGofer := Gofer new.
	revertGofer disablePackageCache.
	gofer repositories do: [ :repo | revertGofer repository: repo ].
	revertGofer version: 'MBFooTests-dkh.3'.
	[ revertGofer load ] on: Warning do: [:ex | ex resume ].

	self assert: (configInfo printString) = '* MBFooTests 1.2(dkh.3)'.
	self assert: ((configInfo asStringOrText isString) or: (configInfo asStringOrText isText)).
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('* MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: ((info asStringOrText isString) or: (info asStringOrText isText))].
