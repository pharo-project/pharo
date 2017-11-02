"
SUnit tests for PharoTutorialHelpTutorialBuilder
"
Class {
	#name : #PharoTutorialHelpTutorialBuilderTest,
	#superclass : #TestCase,
	#category : 'ProfStef-Tests-Tests'
}

{ #category : #tests }
PharoTutorialHelpTutorialBuilderTest >> testBuild [
	| helpTopic tutorial |
	tutorial := PharoTutorialsHelp.
	helpTopic := PharoTutorialHelpTutorialBuilder buildHelpTopicFrom: tutorial.
	self assert: helpTopic notNil
]

{ #category : #tests }
PharoTutorialHelpTutorialBuilderTest >> testInstantiation [ 
	self should: [ PharoTutorialHelpTutorialBuilder new ] raise: Error
]
