Class {
	#name : #SptCatalogProjectsProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#instVars : [
		'firstValue',
		'secondValue'
	],
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptCatalogProjectsProcessorTest >> processor [

	^ SptCatalogProjectsProcessor new
]

{ #category : #tests }
SptCatalogProjectsProcessorTest >> testBeginsWithQueryShowsResults [

	| jsonProject |
	jsonProject := CatalogProvider default projects detect: [:e | e name = 'JSON'].

	self runForText: 'JSON'.
	self assertResultsIncludes: jsonProject.

]

{ #category : #tests }
SptCatalogProjectsProcessorTest >> testEmptyQueryNotShowResults [

	self runForText: ''.
	self assert: candidates results isEmpty

]

{ #category : #tests }
SptCatalogProjectsProcessorTest >> testLookingSubstringShowsResults [

	| jsonProject artefactProject |
	jsonProject := CatalogProvider default projects detect: [:e | e name = 'JSON'].
	artefactProject := CatalogProvider default projects detect: [:e | e name = 'Artefact'].

	self runForText: 'son'.
	self denyResultsIncludes: artefactProject.
	self assertResultsIncludes: jsonProject.

]
