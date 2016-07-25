tests
test0010BasicLoad
	"First test: 
		- make sure that the configuration can be loaded without error
		- make sure that version 1.0 can be loaded without error"
	
	configurationRoot unregister.
	self assert: (Smalltalk at: #ConfigurationOfMBFooTests ifAbsent: []) isNil.
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.
	self assert: (Smalltalk at: #MBFooTestA ifAbsent: []) notNil
