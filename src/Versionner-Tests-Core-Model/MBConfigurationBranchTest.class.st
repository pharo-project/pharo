"
SUnit tests for configuration branches in Versionner
"
Class {
	#name : #MBConfigurationBranchTest,
	#superclass : #TestCase,
	#instVars : [
		'gofer',
		'configurationRoot'
	],
	#category : #'Versionner-Tests-Core-Model'
}

{ #category : #running }
MBConfigurationBranchTest >> hasPackage: aString [
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil
]

{ #category : #running }
MBConfigurationBranchTest >> setUp [

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
MBConfigurationBranchTest >> tearDown [
	configurationRoot ifNotNil: [ configurationRoot unregister ].
	Smalltalk removeKey: #'MB_Foo_Test_Repository' ifAbsent: [].
	gofer := Gofer new.
	self tearDownPackages do: [:pkgName | (self hasPackage: pkgName) ifTrue: [ gofer package: pkgName ]].
	gofer references notEmpty ifTrue: [ gofer unload ].
	super tearDown
]

{ #category : #running }
MBConfigurationBranchTest >> tearDownPackages [

	^#('MBFooTests' 'ConfigurationOfMBBranchTests')
]

{ #category : #tests }
MBConfigurationBranchTest >> test0010BasicLoad [
	"
		- make sure that the configuration can be loaded without error
		- make sure that version 1.0 can be loaded without error"
	
	configurationRoot unregister.
	self assert: (Smalltalk at: #ConfigurationOfMBBranchTests ifAbsent: []) isNil.
	gofer version: 'ConfigurationOfMBBranchTests-dkh.1'.
	gofer load.
	((Smalltalk at: #ConfigurationOfMBBranchTests) project version: '1.0') load.
	self assert: (Smalltalk at: #MBFooTestA ifAbsent: []) notNil

]

{ #category : #tests }
MBConfigurationBranchTest >> test0011BasicLoad [
	"
		- make sure that the branch configuration can be loaded without error
		- make sure that version 1.0 can be loaded without error"
	
	configurationRoot unregister.
	self assert: (Smalltalk at: #ConfigurationOfMBBranchTests ifAbsent: []) isNil.
	gofer version: 'ConfigurationOfMBBranchTests.branch-dkh.2'.
	gofer load.
	((Smalltalk at: #ConfigurationOfMBBranchTests) project version: '1.0') load.
	self assert: (Smalltalk at: #MBFooTestA ifAbsent: []) notNil

]

{ #category : #tests }
MBConfigurationBranchTest >> test002ConfigurationReferences [
	"Make sure that the project and project branches show up in configuration references (for load configuration command)"

	| referenceMap |
	referenceMap := MBLoadConfigurationCommand new configurationReferencesFrom: MBMonticelloPackagesResource current monticelloRepository.
	self assert: (referenceMap keys includes: 'ConfigurationOfMBBranchTests.branch')
]
