tests
test0081BasicVersionLoad
	"8.1:
	 	- load configuration dkh.3
		- load version 1.2
		- validate package and configInfo state
		- edit class
		- validate #recalculate variant for currentVersion"
	| configClass configClassName configInfo expected infos|
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.
	self assert: configInfo currentVersion isNil.
	self assert: configInfo asStringOrText  = 'MBFooTests(dkh.3)'.
 
	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'. 
	self assert: configInfo currentVersion versionString = '1.2'. 
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText  = 'MBFooTests 1.2(dkh.3)'.
	
	"modify class in MBFooTests project"
	(Smalltalk at: #MBFooTestA)
		compile: 'bar ^self' classified: 'mod'.

	self assert: configInfo printString = '* MBFooTests 1.2(dkh.3)'. 
	self assert: configInfo asStringOrText  = '* MBFooTests 1.2(dkh.3)'.
