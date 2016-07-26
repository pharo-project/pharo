tests
test0020BasicConfigurationLoad
	"Second test:
		- load configuration version dkh.1
		- verify that config info added to root
		- validate that all of the data structures are correct .... based on the configuration. "
	| configClassName configClass configInfo baselines versions info packages text |
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (Smalltalk at: configClassName ifAbsent: []) isNil.
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	"configuration"
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo notNil.  
	self assert: configInfo configurationClass == configClass.
	self assert: configInfo isDirty not.
	self assert: configInfo currentVersion isNil.
	self assert: configInfo name = configClassName asString.
	self assert: configInfo validate isEmpty.
	self assert: configInfo projects isEmpty.
	self assert: configInfo packages isEmpty.
	self assert: configInfo groups isEmpty.
	self assert: configInfo printString = 'MBFooTests(dkh.1)'.
	text := configInfo asStringOrText.
	self assert: text isString.
	self assert: text  = 'MBFooTests(dkh.1)'.
	baselines := configInfo baselines.
	self assert: baselines size = 1.
	"baseline"
	info := baselines first.
	self assert: info version blessing == #baseline.
	self assert: info version versionString = '1.0-baseline'.
	self assert: info isDirty not.
	self assert: info validate isEmpty.
	self assert: info projects isEmpty.
	self assert: info groups isEmpty.
	packages := info packages.
	self assert: packages size = 1.
	info := packages first.
	self assert: info isDirty not.
	self assert: info packageName = 'MBFooTests'.
	self assert: (info spec isKindOf: MetacelloPackageSpec). 
	self assert: info spec file = 'MBFooTests'. 
	versions := configInfo versions.
	self assert: versions size = 2.
	"version"
	info := versions first.
	self assert: info version blessing == #release.
	self assert: info version versionString = '1.0'.
	self assert: info isDirty not.
	self assert: info validate isEmpty.
	self assert: info projects isEmpty.
	self assert: info groups isEmpty.
	packages := info packages.
	self assert: packages size = 1.
	info := packages first.
	self assert: info isDirty not.
	self assert: info packageName = 'MBFooTests'.
	self assert: (info spec isKindOf: MetacelloPackageSpec). 
	self assert: info spec file = 'MBFooTests-dkh.1'. 
