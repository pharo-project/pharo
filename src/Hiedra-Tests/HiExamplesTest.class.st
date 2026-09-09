Class {
	#name : 'HiExamplesTest',
	#superclass : 'TestCase',
	#instVars : [
		'example'
	],
	#category : 'Hiedra-Tests-Model',
	#package : 'Hiedra-Tests',
	#tag : 'Model'
}

{ #category : 'running' }
HiExamplesTest >> tearDown [

	example ifNotNil: [ example close ].
	super tearDown
]

{ #category : 'tests' }
HiExamplesTest >> testHiExample [

	example := HiFastTableExample example60RandomCommits 
]

{ #category : 'tests' }
HiExamplesTest >> testHiSpecExamplePresenter [

	example := HiSpecExamplePresenter example60RandomCommits 
]
