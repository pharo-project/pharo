"
SUnit tests for configurations in Versionner
"
Class {
	#name : #MBConfigurationRootTest,
	#superclass : #TestCase,
	#instVars : [
		'gofer',
		'configurationRoot'
	],
	#category : #'Versionner-Tests-Core-Model'
}

{ #category : #tests }
MBConfigurationRootTest >> expectedFailures [ 
	^ #(
		test0021ConfigurationInfoCreated
		test0030BasicVersionLoad 
		test0031ConfigurationInfoChanged
		test0040BasicVersionLoad
		test0050BasicVersionLoad
		test0060BasicVersionLoad
		test0061ConfigurationInfoChanged
		test0062InfoListChanged
		test0070BasicVersionLoad
		test0080BasicVersionLoad
		test0081BasicVersionLoad
		test0082ConfigurationInfoChanged
		test0083InfoListChanged
		test0090BasicVersionLoad
		test0091PackageMismatch
		test0101ConfigurationInfoDeleted
		test0120ConfigPackageSave
	) 
]

{ #category : #running }
MBConfigurationRootTest >> hasPackage: aString [
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil
]

{ #category : #scenarios }
MBConfigurationRootTest >> scenariosForTests [
	"
	1. select a configuration, then unload the configuration ... in OB you gtt an error because the 
		project was undefined .... simulate by creating a ConfigurationOf class that is not a real configuration
	2. add new configuration should associate the chosen repository with the configuration, too...maybe 
		addConfigurationTest should check this
	3. add new configuration then do checkpoing dev and note that dirty * is still on the label in the browser
	"
]

{ #category : #running }
MBConfigurationRootTest >> setUp [

	| repo |
	super setUp.
	gofer := Gofer new.
	gofer disablePackageCache.
	repo := MBMonticelloPackagesResource current monticelloRepository.
	gofer repository: repo.
	Smalltalk at: #'MB_Foo_Test_Repository' put: repo.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	
]

{ #category : #running }
MBConfigurationRootTest >> tearDown [
	configurationRoot ifNotNil: [ configurationRoot unregister ].
	Smalltalk removeKey: #'MB_Foo_Test_Repository' ifAbsent: [].
	gofer := Gofer new.
	self tearDownPackages do: [:pkgName | (self hasPackage: pkgName) ifTrue: [ gofer package: pkgName ]].
	gofer references notEmpty ifTrue: [ gofer unload ].
	super tearDown

]

{ #category : #running }
MBConfigurationRootTest >> tearDownPackages [

	^#('MBFooTests' 'ConfigurationOfMBFooTests')
]

{ #category : #tests }
MBConfigurationRootTest >> test0010BasicLoad [
	"First test: 
		- make sure that the configuration can be loaded without error
		- make sure that version 1.0 can be loaded without error"
	
	configurationRoot unregister.
	self assert: (Smalltalk at: #ConfigurationOfMBFooTests ifAbsent: []) isNil.
	gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
	gofer load.
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.
	self assert: (Smalltalk at: #MBFooTestA ifAbsent: []) notNil

]

{ #category : #tests }
MBConfigurationRootTest >> test0020BasicConfigurationLoad [
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0021ConfigurationInfoCreated [
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.
		configClassName := #ConfigurationOfMBFooTests.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0030BasicVersionLoad [
	"Third test: 
		- load configuration version dkh.1
		- verfy that the target data structures are as expected
		- load version 1.0
		- check that that expected changes to the data structures occur ... in this case that the package is loaded"
	
	| configClass configClassName configInfo versions versionInfo packages packageInfo text |
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0031ConfigurationInfoChanged [
	"If you don't touch the currentVersion of a configurationInfo, then no changed event is signaled on load."
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.

		"load configuration"
		configClassName := #ConfigurationOfMBFooTests.
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0032InfoListChanged [

	| infoListChanged changedValuable |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0040BasicVersionLoad [
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0050BasicVersionLoad [
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0060BasicVersionLoad [
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0061ConfigurationInfoChanged [
	"Touch the currentVersion of a configurationInfo, expect a changed event on load"
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable configInfo configClass |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.

		"load configuration"
		configClassName := #ConfigurationOfMBFooTests.
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		self assert: configInfo currentVersion versionString = '1.0'.
		configInfo packages. "prime the pump"

		"load 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2') load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0062InfoListChanged [
	| infoListChanged changedValuable configClassName configClass configInfo |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		configClassName := #ConfigurationOfMBFooTests.
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load 1.0"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.0') load.

		self deny: infoListChanged.
		infoListChanged := false.

		self assert: configInfo currentVersion versionString = '1.0'.
		configInfo infoList. "prime the pump"

		"load 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2') load.

		self assert: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0070BasicVersionLoad [
	"Seventh test:
	 	- load configuration dkh.3
		- load version 1.0.
		- validate package, baselines and versions state
		- load version 1.2
		- validate new package, baselines and versions state"
	| configClass configClassName configInfo expected infos |
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
	"baselines"
	infos := configInfo baselines.
	self assert: infos size = 1.
	expected := #('1.0-baseline #bleedingEdge').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"versions"
	infos := configInfo versions.
	self assert: infos size = 3.
	expected := #('1.2' '1.1' ' @ 1.0 #stable').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.1').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	"baselines"
	infos := configInfo baselines.
	self assert: infos size = 1.
	expected := #('1.0-baseline #bleedingEdge').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"versions"
	infos := configInfo versions.
	self assert: infos size = 3.
	expected := #(' @ 1.2' '1.1' '1.0 #stable').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].

]

{ #category : #tests }
MBConfigurationRootTest >> test0080BasicVersionLoad [
	"Eighth test:
	 	- load configuration dkh.3
		- load version 1.2
		- validate package and configInfo state
		- edit class and validate"
	| configClass configClassName configInfo expected infos x |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.
	self assert: configInfo currentVersion isNil.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isString.
	"versions"
	infos := configInfo versions.
	self assert: infos size = 3.
	expected := #(' @ 1.2' '1.1' '1.0 #stable').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index)].
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isString ].
	
	"modify class in MBFooTests project"
	(Smalltalk at: #MBFooTestA)
		compile: 'bar ^self' classified: 'mod'.

	self assert: (x := configInfo printString) = '* MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isText.
	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('* MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isText].

]

{ #category : #tests }
MBConfigurationRootTest >> test0081BasicVersionLoad [
	"8.1:
	 	- load configuration dkh.3
		- load version 1.2
		- validate package and configInfo state
		- edit class
		- validate #recalculate variant for currentVersion"
	| configClass configClassName configInfo |
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0082ConfigurationInfoChanged [

	| configInfoDeleted configInfoChanged configInfoCreated createdValuable changedValuable deletedValuable configClassName configClass configInfo |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load version 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		self assert: configInfo currentVersion versionString = '1.2'.
		configInfo packages. "prime the pump"

		"modify class in MBFooTests project"
		(Smalltalk at: #MBFooTestA)
			compile: 'bar ^self' classified: 'mod'.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.
	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0083InfoListChanged [
	| infoListChanged changedValuable configClassName configClass configInfo |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		configClassName := #ConfigurationOfMBFooTests.
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2') load.

		self deny: infoListChanged.
		infoListChanged := false.

		self assert: configInfo currentVersion versionString = '1.2'.
		configInfo infoList. "prime the pump"

		"modify class in MBFooTests project"
		(Smalltalk at: #MBFooTestA)
			compile: 'bar ^self' classified: 'mod'.

		self assert: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0090BasicVersionLoad [
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0091PackageMismatch [
	"
	 	- load configuration dkh.3
		- load version 1.2
		- validate package printString
		- load MBFooTests-dkh.4
		- validate package printString
	"
	| configClass configClassName configInfo expected infos |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	configClass := Smalltalk at: configClassName ifAbsent: [].
	configInfo := configurationRoot configurationInfoFor: configClass.

	"load version 1.1"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: (configInfo printString) = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.

	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isString].

	"load MBFooTests-dkh.4"
	gofer version: 'MBFooTests-dkh.4'.
	gofer load.

	"packages"
	infos := configInfo packages.
	self assert: infos size = 1.
	expected := #('MBFooTests-dkh.3[dkh.4]').
	1 to: infos size do: [:index | | info |
		info := infos at: index.
		self assert: info printString = (expected at: index).
		self assert: info asStringOrText isText].

]

{ #category : #tests }
MBConfigurationRootTest >> test0100BasicConfigurationUnload [
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

]

{ #category : #tests }
MBConfigurationRootTest >> test0101ConfigurationInfoDeleted [
	| createdValuable configClassName configInfoCreated configInfoChanged changedValuable configInfoDeleted deletedValuable configClass configInfo |
	configInfoDeleted := configInfoChanged := configInfoCreated := false.
	createdValuable := [ :ann | configInfoCreated := true ].
	changedValuable := [ :ann | configInfoChanged := true ].
	deletedValuable := [ :ann | configInfoDeleted := true ].
	[ 
		configurationRoot announcer when: MBConfigurationInfoCreated do: createdValuable.
		configurationRoot announcer when: MBConfigurationInfoChanged do: changedValuable.
		configurationRoot announcer when: MBConfigurationInfoDeleted do: deletedValuable.
		configClassName := #ConfigurationOfMBFooTests.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.1'.
		gofer load.

		self assert: configInfoCreated.
		self assert: configInfoChanged.
		self deny: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.

		configClassName := #ConfigurationOfMBFooTests.
		self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
		configInfo := configurationRoot configurationInfoFor: configClass.
		self assert: configInfo printString = 'MBFooTests(dkh.1)'.

		"unload dkh.3"
		gofer unload.

		self deny: configInfoCreated.
		self assert: configInfoChanged.
		self assert: configInfoDeleted.
		configInfoDeleted := configInfoChanged := configInfoCreated := false.
		self assert: (configurationRoot configurations includes: configInfo) not.

	 ] 
			ensure: [ configurationRoot announcer 
					unsubscribe: createdValuable;
					unsubscribe: deletedValuable;
					unsubscribe: changedValuable   ].

]

{ #category : #tests }
MBConfigurationRootTest >> test0110BasicPackageUnload [

	"load dkh.3"
	| configClassName configClass configInfo unloadGofer infos |
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.

	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.
	self assert: configInfo printString = 'MBFooTests(dkh.3)'.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.

	"unload MBFooTests-dkh.3"
	unloadGofer := Gofer new.
	unloadGofer disablePackageCache.
	unloadGofer version: 'MBFooTests-dkh.3'.
	[ unloadGofer unload ] on: Warning do: [:ex | ex resume ].

	self assert: (configInfo printString) = 'MBFooTests(dkh.3)'.
	self assert: configInfo asStringOrText isString.
	"packages"
	infos := configInfo packages.
	self assert: infos size = 0.

]

{ #category : #tests }
MBConfigurationRootTest >> test0111InfoListChanged [
	| infoListChanged changedValuable configClassName configClass configInfo unloadGofer |
	infoListChanged := false.
	changedValuable := [ :ann | infoListChanged := true ].
	[ 
		configurationRoot announcer when: MBInfoListChanged do: changedValuable.

		"load configuration"
		gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
		gofer load.

		self deny: infoListChanged.
		infoListChanged := false.

		configClassName := #ConfigurationOfMBFooTests.
		configClass := Smalltalk at: configClassName ifAbsent: [].
		configInfo := configurationRoot configurationInfoFor: configClass.

		"load version 1.2"
		((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.

		self deny: infoListChanged.
		infoListChanged := false.

		self assert: configInfo currentVersion versionString = '1.2'.
		configInfo infoList. "prime the pump"

		"unload MBFooTests-dkh.3"
		unloadGofer := Gofer new.
		unloadGofer disablePackageCache.
		unloadGofer version: 'MBFooTests-dkh.3'.
		[ unloadGofer unload ] on: Warning do: [:ex | ex resume ].

		self assert: infoListChanged.
	 ] 
			ensure: [ configurationRoot announcer unsubscribe: changedValuable   ].


]

{ #category : #tests }
MBConfigurationRootTest >> test0120ConfigPackageSave [
	"Issue 125 test
	 	- load configuration dkh.3
		- load version 1.2
		- modify configuration and validate
		- save configuration package and validate"
	| configClass configClassName configInfo suggestedName signature |
	"load configuration dkh.3"
	gofer version: 'ConfigurationOfMBFooTests-dkh.3'.
	gofer load.
	configClassName := #ConfigurationOfMBFooTests.
	self assert: (configClass := Smalltalk at: configClassName ifAbsent: []) notNil.
	configInfo := configurationRoot configurationInfoFor: configClass.

	"load version 1.2"
	((Smalltalk at: #ConfigurationOfMBFooTests) project version: '1.2')  load.
	self assert: configInfo workingCopy notNil. 
	
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo currentVersion versionString = '1.2'.
	self assert: configInfo printString = 'MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isString.
	
	"modify class in ConfigurationOfMBFooTests class"
	(Smalltalk at: #ConfigurationOfMBFooTests)
		compile: 'bar ^self' classified: 'mod'.

	self assert: configInfo printString = '* MBFooTests 1.2(dkh.3)'.
	self assert: configInfo asStringOrText isText.

	"save the configuraiton package"
	[ Gofer new
		disablePackageCache;
		repository: (MCDictionaryRepository new);
		package: configClassName asString;
		commit ] 
			on: MCVersionNameAndMessageRequest
			do: [:ex |  
				suggestedName := ex suggestedName.
				ex resume: { suggestedName.  'commit comment'}].

	signature := suggestedName copyAfterLast: $-.
	self assert: configInfo printString = ('MBFooTests 1.2(', signature, ')').
	self assert: configInfo asStringOrText isString.

]
