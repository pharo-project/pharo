"
SUnit tests for tutorial navigation
"
Class {
	#name : #PharoTutorialGoOnMockTutorialTest,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #running }
PharoTutorialGoOnMockTutorialTest >> setUp [
	super setUp.
	ProfStef tutorial: MockTutorial lesson: #firstLesson 
]

{ #category : #running }
PharoTutorialGoOnMockTutorialTest >> tearDown [
	ProfStef default close.
	super tearDown
]

{ #category : #tests }
PharoTutorialGoOnMockTutorialTest >> testFirstLessonShouldBeDisplayed [
 	self assert:  'First lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialGoOnMockTutorialTest >> testLastShouldGoToThirdLesson [
	ProfStef last.
 	self assert:  'Third lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialGoOnMockTutorialTest >> testLastThenFirstShouldGoToFirstLesson [
	ProfStef last; first.
 	self assert:  'First lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialGoOnMockTutorialTest >> testLastThenPreviousShouldGoToSecondLesson [
	ProfStef last; previous.
 	self assert:  'Second lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialGoOnMockTutorialTest >> testNextShouldGoToSecondLesson [
	ProfStef next.
 	self assert:  'Second lesson' equals: ProfStef default lessonView text
]
