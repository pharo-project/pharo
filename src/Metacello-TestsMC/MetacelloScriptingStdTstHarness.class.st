Class {
	#name : #MetacelloScriptingStdTstHarness,
	#superclass : #TestCase,
	#instVars : [
		'registry',
		'undefinedSymbols',
		'monticelloRepository',
		'configurationRepository',
		'initialWorkingCopyList'
	],
	#category : #'Metacello-TestsMC'
}

{ #category : #running }
MetacelloScriptingStdTstHarness >> defaultTimeout [
	"I don't want no stkinkin' timeouts"
	^60000
]

{ #category : #running }
MetacelloScriptingStdTstHarness >> disableUndefinedSymbolTracking [
  "significant perfomance improvement"

  ^ true
]

{ #category : #running }
MetacelloScriptingStdTstHarness >> doSilently [
    ^ true
]

{ #category : #utilities }
MetacelloScriptingStdTstHarness >> hasPackage: aString [
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil
]

{ #category : #running }
MetacelloScriptingStdTstHarness >> runCase [
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
MetacelloScriptingStdTstHarness >> setUp [
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
MetacelloScriptingStdTstHarness >> setUpRepositories [

]

{ #category : #running }
MetacelloScriptingStdTstHarness >> setUpRepositoryContents [

]

{ #category : #running }
MetacelloScriptingStdTstHarness >> tearDown [
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
      Transcript
        cr;
        show:
            'leaked package from ' , self printString , ' -> ' , leak printString ].
  self assert: diff isEmpty.
  super tearDown
]

{ #category : #running }
MetacelloScriptingStdTstHarness >> tearDownPackageList [
    ^ #()
]

{ #category : #running }
MetacelloScriptingStdTstHarness >> tearDownPackages [
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
MetacelloScriptingStdTstHarness >> tearDownRepositories [

]

{ #category : #running }
MetacelloScriptingStdTstHarness >> unloadPackage: packageName [
  | aGofer |
  aGofer := Gofer new.
  (self hasPackage: packageName)
    ifTrue: [ aGofer package: packageName ].
  aGofer references notEmpty
    ifTrue: [ aGofer unload ]
]

{ #category : #utilities }
MetacelloScriptingStdTstHarness >> validate: expectedConfigurationClassName expConfigVersion: expectedConfigurationVersion expConfigRepo: expectedConfigurationRepository expBaselineClassName: expectedBaselineClassName expBaselineVersion: expectedBaselineVersion expBaselineRepo: expectedBaselineRepository [
  MetacelloProjectRegistration registry configurationRegistry
    at: expectedConfigurationClassName
    ifPresent: [ :existing | 
      | spec x |
      spec := existing configurationProjectSpec.
      self
        assert: (x := spec version versionString) = expectedConfigurationVersion.
      self
        assert:
          (spec repositoryDescriptions includes: expectedConfigurationRepository) ]
    ifAbsent: [ self assert: expectedConfigurationVersion == nil ].
  MetacelloProjectRegistration registry baselineRegistry
    at: expectedBaselineClassName
    ifPresent: [ :existing | 
      | spec |
      spec := existing baselineProjectSpec.
      self assert: spec versionString = expectedBaselineVersion.
      self
        assert:
          (spec repositoryDescriptions includes: expectedBaselineRepository) ]
    ifAbsent: [ self assert: expectedBaselineVersion == nil ]
]

{ #category : #utilities }
MetacelloScriptingStdTstHarness >> validateProjects: specArrays [
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
MetacelloScriptingStdTstHarness >> verify: packageName loadedFrom: repositoryDescription [
    | externalCoreWorkingCopy |
    externalCoreWorkingCopy := MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ].
    self
        assert:
            (externalCoreWorkingCopy repositoryGroup repositories
                includes: (MetacelloMCProject new repositorySpec description: repositoryDescription) createRepository)
]

{ #category : #utilities }
MetacelloScriptingStdTstHarness >> verify: packageName version: fileName [
    | externalCoreWorkingCopy x |
    externalCoreWorkingCopy := MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ].
    self assert: (x := externalCoreWorkingCopy ancestors first name) = fileName
]

{ #category : #utilities }
MetacelloScriptingStdTstHarness >> verifyPackageNotLoaded: packageName [
    self assert: (MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ] ifNone: [  ]) == nil
]
