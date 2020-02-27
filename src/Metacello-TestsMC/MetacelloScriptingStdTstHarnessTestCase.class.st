Class {
	#name : #MetacelloScriptingStdTstHarnessTestCase,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'registry',
		'undefinedSymbols',
		'initialWorkingCopyList'
	],
	#category : #'Metacello-TestsMC'
}

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> defaultTimeout [
	"I don't want no stkinkin' timeouts"
	^60000
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> disableUndefinedSymbolTracking [
  "significant perfomance improvement"

  ^ true
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> doSilently [
    ^ true
]

{ #category : #utilities }
MetacelloScriptingStdTstHarnessTestCase >> hasPackage: aString [

	^ MCWorkingCopy allManagers
		anySatisfy: [ :each | each packageName = aString ]
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> runCase [
    | original |
    self doSilently
        ifFalse: [ ^ super runCase ].
    original := MetacelloPlatform current bypassGoferLoadUpdateCategories.
    [ 
    MetacelloPlatform current bypassGoferLoadUpdateCategories: true.
    ^ MetacelloPlatform current suspendSystemUpdateEventsDuring: [ super runCase ] ]
        ensure: [ MetacelloPlatform current bypassGoferLoadUpdateCategories: original ]
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> setUp [
  super setUp.
  MetacelloPlatform current clearPackageCache.
  registry := MetacelloProjectRegistration registry.
  self setUpRepositories.
  self setUpRepositoryContents.
  MetacelloProjectRegistration resetRegistry.
  initialWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ].
  self disableUndefinedSymbolTracking
    ifTrue: [ undefinedSymbols := MetacelloPlatform current disableUndefinedSybolUpdates ]
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> setUpRepositories [

]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> setUpRepositoryContents [

]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> tearDown [
  | finalWorkingCopyList diff |  
  self tearDownPackages.
  self tearDownRepositories.
  MetacelloProjectRegistration registry: registry.
  self disableUndefinedSymbolTracking
    ifTrue: [ MetacelloPlatform current reenableUndefinedSybolUpdates: undefinedSymbols ].
  finalWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ].
  diff := finalWorkingCopyList difference: initialWorkingCopyList.
  diff
    do: [ :leak | 
      MetacelloNotification signal: ('leaked package from ' , self printString , ' -> ' , leak printString) ].
  self assert: diff isEmpty.
  super tearDown

]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> tearDownPackageList [
    ^ #()
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> tearDownPackages [
    | aGofer |
    aGofer := Gofer new.
    self tearDownPackageList
        do: [ :packageName | 
            (self hasPackage: packageName)
                ifTrue: [ aGofer package: packageName ] ].
    aGofer references notEmpty
        ifTrue: [ aGofer metacelloUnload ]
]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> tearDownRepositories [

]

{ #category : #running }
MetacelloScriptingStdTstHarnessTestCase >> unloadPackage: packageName [
  | aGofer |
  aGofer := Gofer new.
  (self hasPackage: packageName)
    ifTrue: [ aGofer package: packageName ].
  aGofer references notEmpty
    ifTrue: [ aGofer unload ]
]

{ #category : #utilities }
MetacelloScriptingStdTstHarnessTestCase >> validate: expectedConfigurationClassName expConfigVersion: expectedConfigurationVersion expConfigRepo: expectedConfigurationRepository expBaselineClassName: expectedBaselineClassName expBaselineVersion: expectedBaselineVersion expBaselineRepo: expectedBaselineRepository [
	MetacelloProjectRegistration registry configurationRegistry
		at: expectedConfigurationClassName
		ifPresent: [ :existing | 
			| spec x |
			spec := existing configurationProjectSpec.
			self assert: (x := spec version versionString) equals: expectedConfigurationVersion.
			self assert: (spec repositoryDescriptions includes: expectedConfigurationRepository) ]
		ifAbsent: [ self assert: expectedConfigurationVersion identicalTo: nil ].
	MetacelloProjectRegistration registry baselineRegistry
		at: expectedBaselineClassName
		ifPresent: [ :existing | 
			| spec |
			spec := existing baselineProjectSpec.
			self assert: spec versionString equals: expectedBaselineVersion.
			self assert: (spec repositoryDescriptions includes: expectedBaselineRepository) ]
		ifAbsent: [ self assert: expectedBaselineVersion identicalTo: nil ]
]

{ #category : #utilities }
MetacelloScriptingStdTstHarnessTestCase >> validateProjects: specArrays [
  specArrays
    do: [ :array | 
      self
        validate: (array at: 1)
        expConfigVersion: (array at: 2)
        expConfigRepo: (array at: 3)
        expBaselineClassName: (array at: 4)
        expBaselineVersion: (array at: 5)
        expBaselineRepo: (array at: 6) ]
]

{ #category : #utilities }
MetacelloScriptingStdTstHarnessTestCase >> verify: packageName loadedFrom: repositoryDescription [
    | externalCoreWorkingCopy |
    externalCoreWorkingCopy := MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ].
    self
        assert:
            (externalCoreWorkingCopy repositoryGroup repositories
                includes: (MetacelloMCProject new repositorySpec description: repositoryDescription) createRepository)
]

{ #category : #utilities }
MetacelloScriptingStdTstHarnessTestCase >> verify: packageName version: fileName [
	| externalCoreWorkingCopy x |
	externalCoreWorkingCopy := MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ].
	self assert: (x := externalCoreWorkingCopy ancestors first name) equals: fileName
]

{ #category : #utilities }
MetacelloScriptingStdTstHarnessTestCase >> verifyPackageNotLoaded: packageName [
	self assert: (MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ] ifNone: [  ]) identicalTo: nil
]
