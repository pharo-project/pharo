"
SUnit tests for tutorial player
"
Class {
	#name : #TutorialPlayerTestWithMockTutorial,
	#superclass : #TestCase,
	#instVars : [
		'player'
	],
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #running }
TutorialPlayerTestWithMockTutorial >> setUp [
	| tutorial |
	super setUp.
	tutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: tutorial 
]

{ #category : #tests }
TutorialPlayerTestWithMockTutorial >> testCurrentLessonIsFirstOneAtCreation [
	self assert: player currentLesson title equals: 'first'.
]

{ #category : #tests }
TutorialPlayerTestWithMockTutorial >> testNavigation [
	self assert: player next title equals: 'second'.
	self assert: player currentLesson title equals: 'second'.
	
	self assert: player next title equals: 'third'.
	self assert: player currentLesson title equals: 'third'.
	
	self assert: player next title equals: 'third'.
	self assert: player currentLesson title equals: 'third'.
	
	self assert: player previous title equals: 'second'.
	self assert: player currentLesson title equals: 'second'.
	
	self assert: player previous title equals: 'first'.
	self assert: player currentLesson title equals: 'first'.
	
	self assert: player previous title equals: 'first'.
	self assert: player currentLesson title equals: 'first'.
]

{ #category : #tests }
TutorialPlayerTestWithMockTutorial >> testResetTutorialGoBackToFirstLesson [
	player next; next.
	self assert: player currentLesson title equals: 'third'.
	
	player rewind.
	self assert: player currentLesson title equals: 'first'.
]

{ #category : #tests }
TutorialPlayerTestWithMockTutorial >> testSizeReturnsThree [
	self assert: player size equals: 3.
]
