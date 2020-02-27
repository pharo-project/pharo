"
SUnit tests for PharoTutorialHelp
"
Class {
	#name : #PharoTutorialHelpTest,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #tests }
PharoTutorialHelpTest >> testBookName [
	self assert: PharoTutorialHelp bookName isString
]

{ #category : #tests }
PharoTutorialHelpTest >> testCreateATutorial [
	| helpTopic |
	helpTopic := PharoTutorialHelp createATutorial.
	self assert: helpTopic notNil.
	self assert: helpTopic class identicalTo: HelpTopic.
	self assert: helpTopic title equals: 'Create a tutorial'
]

{ #category : #tests }
PharoTutorialHelpTest >> testIntroduction [
	| helpTopic |
	helpTopic := PharoTutorialHelp introduction.
	self assert: helpTopic notNil.
	self assert: helpTopic class identicalTo: HelpTopic.
	self assert: helpTopic title equals: 'Introduction'
]

{ #category : #tests }
PharoTutorialHelpTest >> testListOfTutorials [
	| helpTopic |
	helpTopic := PharoTutorialHelp listOfTutorials.
	self assert: helpTopic notNil.
	self assert: helpTopic title equals: 'List of tutorials'
]

{ #category : #tests }
PharoTutorialHelpTest >> testPages [
	self assert: PharoTutorialHelp pages isCollection.
	self assert: (PharoTutorialHelp pages allSatisfy: #isSymbol).
]
