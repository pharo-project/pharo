running
setUpRepositories
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
  Smalltalk
    at: 'Metacello_Configuration_Test_Repository' asSymbol
    put: externalRepository.
  monticelloRepository := MetacelloMonticelloResource current
    monticelloRepository.
  Smalltalk
    at: 'Metacello_Gofer_Test_Repository' asSymbol
    put: monticelloRepository.
  mczConfigurationRepository := MetacelloConfigurationResource current
    monticelloRepository.
  Smalltalk
    at: 'Metacello_MczConfiguration_Test_Repository' asSymbol
    put: mczConfigurationRepository