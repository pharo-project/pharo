"
SUnit tests for tutorial navigation
"
Class {
	#name : #PharoTutorialTestGoOnMockTutorial,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #running }
PharoTutorialTestGoOnMockTutorial >> setUp [
	super setUp.
	ProfStef tutorial: MockTutorial lesson: #firstLesson 
]

{ #category : #running }
PharoTutorialTestGoOnMockTutorial >> tearDown [
	ProfStef default close.
	super tearDown
]

{ #category : #tests }
PharoTutorialTestGoOnMockTutorial >> testFirstLessonShouldBeDisplayed [
 	self assert:  'First lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialTestGoOnMockTutorial >> testLastShouldGoToThirdLesson [
	ProfStef last.
 	self assert:  'Third lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialTestGoOnMockTutorial >> testLastThenFirstShouldGoToFirstLesson [
	ProfStef last; first.
 	self assert:  'First lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialTestGoOnMockTutorial >> testLastThenPreviousShouldGoToSecondLesson [
	ProfStef last; previous.
 	self assert:  'Second lesson' equals: ProfStef default lessonView text
]

{ #category : #tests }
PharoTutorialTestGoOnMockTutorial >> testNextShouldGoToSecondLesson [
	ProfStef next.
 	self assert:  'Second lesson' equals: ProfStef default lessonView text
]
