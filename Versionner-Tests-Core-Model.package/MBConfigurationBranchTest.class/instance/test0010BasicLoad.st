tests
test0010BasicLoad
	"
		- make sure that the configuration can be loaded without error
		- make sure that version 1.0 can be loaded without error"
	
	configurationRoot unregister.
	self assert: (Smalltalk at: #ConfigurationOfMBBranchTests ifAbsent: []) isNil.
	gofer version: 'ConfigurationOfMBBranchTests-dkh.1'.
	gofer load.
	((Smalltalk at: #ConfigurationOfMBBranchTests) project version: '1.0') load.
	self assert: (Smalltalk at: #MBFooTestA ifAbsent: []) notNil
