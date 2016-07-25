tests
test0100BasicConfigurationUnload
	"10:
	 	- load configuration dkh.3
		- unload configuration"

	"load dkh.3"
	| configClassName configClass configInfo |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.

	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.

	"unload dkh.3"
	gofer unload.

	self assert: (configurationRoot configurations includes: configInfo) not.
