Class {
	#name : #MetacelloExternalScriptingTestCase,
	#superclass : #MetacelloBasicScriptingTestCase,
	#category : #Metacello-TestsMC
}

{ #category : #running }
MetacelloExternalScriptingTestCase >> setUpRepositoryContents [
    super setUpRepositoryContents.
    Smalltalk at: #'Metacello_Config_Test_Repository' put: configurationRepository
]

{ #category : #running }
MetacelloExternalScriptingTestCase >> tearDownRepositories [
  super tearDownRepositories.
  MCRepositoryGroup default
    removeIdenticalRepository:
      (Smalltalk at: #'Metacello_XXX_Test_Repository' ifAbsent: [  ]).
  Smalltalk globals removeKey: #'Metacello_Config_Test_Repository' ifAbsent: [  ].
  Smalltalk globals removeKey: #'Metacello_XXX_Test_Repository' ifAbsent: [  ]
]
