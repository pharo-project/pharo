tests
test0060BasicVersionLoad
	"Sixth test:
	 	- load configuration dkh.3
		- send currentVersion to configInfo ... which record currentVersion
		- load version 1.0 and currentVersion should be updated to reflect fact that pacakges loaded.
		- load version 1.2 and verify"
	| configClass configClassName configInfo |
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


	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
