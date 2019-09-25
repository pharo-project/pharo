Class {
	#name : #MetacelloExternalScriptingTestCase,
	#superclass : #MetacelloBasicScriptingTestCase,
	#category : #'Metacello-TestsMC'
}

{ #category : #running }
MetacelloExternalScriptingTestCase >> setUpRepositoryContents [
    super setUpRepositoryContents.
    testingEnvironment at: #'Metacello_Config_Test_Repository' put: configurationRepository
]

{ #category : #running }
MetacelloExternalScriptingTestCase >> tearDownRepositories [
  super tearDownRepositories.
  MCRepositoryGroup default
    removeIdenticalRepository:
      (testingEnvironment at: #'Metacello_XXX_Test_Repository' ifAbsent: [  ]).
  testingEnvironment removeKey: #'Metacello_Config_Test_Repository' ifAbsent: [  ].
  testingEnvironment removeKey: #'Metacello_XXX_Test_Repository' ifAbsent: [  ]
]
