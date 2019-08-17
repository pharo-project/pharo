"
SUnit tests for tutorial player
"
Class {
	#name : #TutorialPlayerWithMockTutorialTest,
	#superclass : #TestCase,
	#instVars : [
		'player'
	],
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #running }
TutorialPlayerWithMockTutorialTest >> setUp [
	| tutorial |
	super setUp.
	tutorial := MockTutorial new.
	player := TutorialPlayer new tutorial: tutorial 
]

{ #category : #tests }
TutorialPlayerWithMockTutorialTest >> testCurrentLessonIsFirstOneAtCreation [
	self assert: player currentLesson title equals: 'first'.
]

{ #category : #tests }
TutorialPlayerWithMockTutorialTest >> testNavigation [
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
TutorialPlayerWithMockTutorialTest >> testResetTutorialGoBackToFirstLesson [
	player next; next.
	self assert: player currentLesson title equals: 'third'.
	
	player rewind.
	self assert: player currentLesson title equals: 'first'.
]

{ #category : #tests }
TutorialPlayerWithMockTutorialTest >> testSizeReturnsThree [
	self assert: player size equals: 3.
]
