Class {
	#name : #MetacelloExternalScriptingTestCase,
	#superclass : #MetacelloBasicScriptingTestCase,
	#category : #'Metacello-TestsMC'
}

{ #category : #running }
MetacelloExternalScriptingTestCase >> setUpRepositoryContents [
    super setUpRepositoryContents.
    aTestEnvironment at: #'Metacello_Config_Test_Repository' put: configurationRepository
]

{ #category : #running }
MetacelloExternalScriptingTestCase >> tearDownRepositories [
  super tearDownRepositories.
  MCRepositoryGroup default
    removeIdenticalRepository:
      (aTestEnvironment at: #'Metacello_XXX_Test_Repository' ifAbsent: [  ]).
  aTestEnvironment removeKey: #'Metacello_Config_Test_Repository' ifAbsent: [  ].
  aTestEnvironment removeKey: #'Metacello_XXX_Test_Repository' ifAbsent: [  ]
]
