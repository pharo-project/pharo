"
##MetacelloScriptingTestCase

**MetacelloScriptingTestCase** implements the test suite that validates the **Metacello** scripting API.

"
Class {
	#name : #MetacelloScriptingTestCase,
	#superclass : #MetacelloScriptingStdTstHarnessTestCase,
	#instVars : [
		'externalRepository',
		'mczConfigurationRepository',
		'configurationDirectory',
		'externalDirectory',
		'sampleDirectory',
		'configurationRepository',
		'monticelloRepository'
	],
	#category : #'Metacello-TestsMC'
}

{ #category : #accessing }
MetacelloScriptingTestCase >> configurationPath [
    ^ 'metacello-tests/configuration'
]

{ #category : #accessing }
MetacelloScriptingTestCase >> externalPath [
    ^ 'metacello-tests/external'
]

{ #category : #accessing }
MetacelloScriptingTestCase >> samplePath [
    ^ 'metacello-tests/sample'
]

{ #category : #running }
MetacelloScriptingTestCase >> setUpRepositories [
  | defaultDirectory |
  super setUpRepositories.
  defaultDirectory := MetacelloPlatform current defaultDirectory.
  configurationDirectory := MetacelloPlatform current
    directoryFromPath: self configurationPath
    relativeTo: defaultDirectory.
  externalDirectory := MetacelloPlatform current
    directoryFromPath: self externalPath
    relativeTo: defaultDirectory.
  sampleDirectory := MetacelloPlatform current
    directoryFromPath: self samplePath
    relativeTo: defaultDirectory.
  configurationDirectory exists
    ifTrue: [ MetacelloPlatform current recursiveDelete: configurationDirectory ].
  MetacelloPlatform current ensureDirectoryExists: configurationDirectory.
  externalDirectory exists
    ifTrue: [ MetacelloPlatform current recursiveDelete: externalDirectory ].
  MetacelloPlatform current ensureDirectoryExists: externalDirectory.
  sampleDirectory exists
    ifTrue: [ MetacelloPlatform current recursiveDelete: sampleDirectory ].
  MetacelloPlatform current ensureDirectoryExists: sampleDirectory.
  configurationRepository := MetacelloScriptingResource current
    configurationRepository.
  externalRepository := MetacelloScriptingResource current externalRepository.
  testingEnvironment
    at: 'Metacello_Configuration_Test_Repository' asSymbol
    put: externalRepository.
  monticelloRepository := MetacelloMonticelloResource current
    monticelloRepository.
  testingEnvironment
    at: 'Metacello_Gofer_Test_Repository' asSymbol
    put: monticelloRepository.
  mczConfigurationRepository := MetacelloConfigurationResource current
    monticelloRepository.
  testingEnvironment
    at: 'Metacello_MczConfiguration_Test_Repository' asSymbol
    put: mczConfigurationRepository
]

{ #category : #running }
MetacelloScriptingTestCase >> setUpRepositoryContents: packageList sourceRepo: sourceRepo targetRepo: targetRepo [
    packageList
        do: [ :package | 
            | versionInfo |
            versionInfo := sourceRepo versionInfoFromVersionNamed: package.
            targetRepo storeVersion: (sourceRepo versionWithInfo: versionInfo ifAbsent: [ self assert: false ]) ]
]

{ #category : #running }
MetacelloScriptingTestCase >> tearDownRepositories [
  super tearDownRepositories.
  configurationDirectory exists
    ifTrue: [ MetacelloPlatform current recursiveDelete: configurationDirectory ].
  externalDirectory exists
    ifTrue: [ MetacelloPlatform current recursiveDelete: externalDirectory ].
  sampleDirectory exists
    ifTrue: [ MetacelloPlatform current recursiveDelete: sampleDirectory ].
  testingEnvironment removeKey: #'Metacello_Configuration_Test_Repository' ifAbsent: [  ].
  testingEnvironment removeKey: #'Metacello_Gofer_Test_Repository' ifAbsent: [  ].
  testingEnvironment
    removeKey: #'Metacello_MczConfiguration_Test_Repository'
    ifAbsent: [  ].
  MCRepositoryGroup default
    removeIdenticalRepository: externalRepository;
    removeIdenticalRepository: monticelloRepository;
    removeIdenticalRepository: mczConfigurationRepository;
    removeIdenticalRepository: configurationRepository
]
