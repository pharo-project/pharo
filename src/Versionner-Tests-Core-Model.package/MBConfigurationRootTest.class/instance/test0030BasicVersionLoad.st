tests
test0030BasicVersionLoad
	"Third test: 
		- load configuration version dkh.1
		- verfy that the target data structures are as expected
		- load version 1.0
		- check that that expected changes to the data structures occur ... in this case that the package is loaded"
	
	| configClass configClassName configInfo versions versionInfo packages packageInfo text x |
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.1)'.
	text := configInfo asStringOrText.
	self assert: text isString.
	self assert: text  = 'MBFooTests(dkh.1)'.
	versions := configInfo versions.
	self assert: versions size = 1.
	versionInfo := versions first.
	packages := versionInfo packages.
	self assert: packages size = 1.
	packageInfo := packages first.
	self assert: packageInfo workingCopy isNil.
	self assert: packageInfo printString = 'MBFooTests ---'.
	text := packageInfo asStringOrText.
	self assert: text isString.
	self assert: text  = 'MBFooTests ---'.

	"load version 1.0"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.1)'.
	self assert: configInfo currentVersion versionString = '1.0'. 
	self assert: configInfo printString = 'MBFooTests 1.0(dkh.1)'.
	text := configInfo asStringOrText.
	self assert: text isString.
	self assert: text  = 'MBFooTests 1.0(dkh.1)'.
	versions := configInfo versions.
	self assert: versions size = 1.
	"reacquire the packageInfo instance, since the old instance is not valid"
	versionInfo := versions first.
	packages := versionInfo packages.
	self assert: packages size = 1.
	packageInfo := packages first.
	self assert: packageInfo workingCopy notNil.
	self assert: packageInfo printString = 'MBFooTests-dkh.1'.
	text := packageInfo asStringOrText.
	self assert: text isString.
	self assert: text  = 'MBFooTests-dkh.1'.
