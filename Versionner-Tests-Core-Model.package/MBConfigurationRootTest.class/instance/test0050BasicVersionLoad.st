tests
test0050BasicVersionLoad
	"Fifth test:
	 	- load configuration dkh.2(no events when loading version 1.1!)
		- send currentVersion to configInfo ... which record currentVersion
		- load version 1.0 and currentVersion should be updated to reflect fact that pacakges loaded.
		- load version 1.1 and verify"
	| configClass configClassName configInfo |
	gofer version: 'ConfigurationOfMBFooTests-dkh.2'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.2)'.
	self assert: configInfo currentVersion isNil.

	"load version 1.0"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.2)'.
	self assert: configInfo currentVersion versionString = '1.0'.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.2)'.

	"load version 1.1"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.1')  load.
	self flag: '...NOTE dkh.2 is identical to dkh.1and no events generated on load'.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.2)'.
