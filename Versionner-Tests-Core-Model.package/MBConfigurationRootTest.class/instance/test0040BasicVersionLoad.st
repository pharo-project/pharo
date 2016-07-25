tests
test0040BasicVersionLoad
	"Fourth test:
	 	- load configuration dkh.1
		- send currentVersion to configInfo ... which record currentVersion
		- load version 1.0 and currentVersion should be updated to reflect fact that pacakges loaded."
	| configClass configClassName configInfo |
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.1)'.
	self assert: configInfo currentVersion isNil.

	"load version 1.0"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.1)'.
	self assert: configInfo currentVersion versionString = '1.0'. 
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.1)'.
