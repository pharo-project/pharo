Class {
	#name : #SptHelpProcessorTest,
	#superclass : #AbstractSptProcessorTest,
	#instVars : [
		'firstValue',
		'secondValue'
	],
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #tests }
SptHelpProcessorTest >> assertResultsIncludesHelpTopic: aHelpTopic [

	^ self assert: (candidates results
				anySatisfy: [ :each | each content title = aHelpTopic title ])
]

{ #category : #tests }
SptHelpProcessorTest >> denyResultsIncludesHelpTopic: aHelpTopic [

	^ self assert: (candidates results
				noneSatisfy: [ :each | each content title = aHelpTopic title ])
]

{ #category : #tests }
SptHelpProcessorTest >> processor [

	^ SptHelpProcessor new
]

{ #category : #tests }
SptHelpProcessorTest >> testBeginsWithQueryShowsResults [

	self runForText: 'Contr'.
	self assertResultsIncludesHelpTopic: HowToContributeHelp asHelpTopic.

]

{ #category : #tests }
SptHelpProcessorTest >> testEmptyQueryShowsResults [

	self runForText: ''.
	self assertResultsIncludesHelpTopic: WelcomeHelp asHelpTopic.

]

{ #category : #tests }
SptHelpProcessorTest >> testLookingSubstringShowsResults [

	self runForText: 'menu'.
	self denyResultsIncludesHelpTopic: HowToContributeHelp asHelpTopic.
	self assertResultsIncludesHelpTopic: WorldMenuHelp asHelpTopic.

]
