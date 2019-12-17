"
SUnit tests for tutorial navigation
"
Class {
	#name : #PharoTutorialGoTest,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #running }
PharoTutorialGoTest >> tearDown [
	ProfStef default close.
	super tearDown
]

{ #category : #tests }
PharoTutorialGoTest >> testGoOnMockTutorial [
	| displayedText expected |
	ProfStef goOn: MockTutorial.
	
	displayedText := ProfStef default lessonView text.
	expected := MockTutorial new firstLesson lesson.
 	self assert: displayedText equals: expected.
]

{ #category : #tests }
PharoTutorialGoTest >> testGoOpenSmalltalkSyntaxTutorial [
	| displayedText expected |
	ProfStef go.
	
	displayedText := ProfStef default lessonView text.
	expected := PharoSyntaxTutorial new welcome lesson.
 	self assert: displayedText equals: expected.
]

{ #category : #tests }
PharoTutorialGoTest >> testGoTwiceShowLessonViewIfNotVisible [
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	firstLessonView close.
	ProfStef goOn: PharoSyntaxTutorial.
	self assert:  (self currentWorld systemWindows includes:  firstLessonView window).
]

{ #category : #tests }
PharoTutorialGoTest >> testGoTwiceUseSameLessonView [
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	ProfStef goOn: PharoSyntaxTutorial.
	self assert: firstLessonView identicalTo: ProfStef default lessonView
]
