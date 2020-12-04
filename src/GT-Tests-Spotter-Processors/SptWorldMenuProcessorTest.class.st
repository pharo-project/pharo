Class {
	#name : #SptWorldMenuProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #asserting }
SptWorldMenuProcessorTest >> assertResultsIncludesMenuEntry: aString [ 
	
	self assert: (candidates results anySatisfy: [:each | each content contents = aString ])
]

{ #category : #asserting }
SptWorldMenuProcessorTest >> denyResultsIncludesMenuEntry: aString [ 
	
	self assert: (candidates results noneSatisfy: [:each | each content contents = aString ])
]

{ #category : #tests }
SptWorldMenuProcessorTest >> processor [

	^ SptWorldMenuProcessor new
]

{ #category : #tests }
SptWorldMenuProcessorTest >> testEmptyQueryShowsResults [

	self runForText: ''.
	self assertResultsIncludesMenuEntry: 'Iceberg'.

]

{ #category : #tests }
SptWorldMenuProcessorTest >> testSearchingSaDoesNotIncludeIceberg [

	self runForText: 'sa'.
	self denyResultsIncludesMenuEntry: 'Iceberg'.

]

{ #category : #tests }
SptWorldMenuProcessorTest >> testSearchingSaIncludesSaveAndSaveAs [

	self runForText: 'sa'.
	self assertResultsIncludesMenuEntry: 'Save'.
	self assertResultsIncludesMenuEntry: 'Save as...'	
]

{ #category : #tests }
SptWorldMenuProcessorTest >> testSearchingSettingsFindsTheCorrectEntry [

	self runForText: 'settings'.
	self assertResultsIncludesMenuEntry: 'Settings'
]

{ #category : #tests }
SptWorldMenuProcessorTest >> testSearchingVeDoesNotIncludeIceberg [

	self runForText: 've'.
	self denyResultsIncludesMenuEntry: 'Iceberg'.

]

{ #category : #tests }
SptWorldMenuProcessorTest >> testSearchingVeIncludesSaveAndSaveAs [

	self runForText: 'Ve'.
	self assertResultsIncludesMenuEntry: 'Save'.
	self assertResultsIncludesMenuEntry: 'Save as...'	
]
