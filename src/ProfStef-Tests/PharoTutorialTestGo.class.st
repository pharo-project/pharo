"
SUnit tests for tutorial navigation
"
Class {
	#name : #PharoTutorialTestGo,
	#superclass : #TestCase,
	#category : #'ProfStef-Tests-Tests'
}

{ #category : #running }
PharoTutorialTestGo >> tearDown [
	ProfStef default close.
	super tearDown
]

{ #category : #tests }
PharoTutorialTestGo >> testGoOnMockTutorial [
	| displayedText expected |
	ProfStef goOn: MockTutorial.
	
	displayedText := ProfStef default lessonView text.
	expected := MockTutorial new firstLesson lesson.
 	self assert: displayedText equals: expected.
]

{ #category : #tests }
PharoTutorialTestGo >> testGoOpenSmalltalkSyntaxTutorial [
	| displayedText expected |
	ProfStef go.
	
	displayedText := ProfStef default lessonView text.
	expected := PharoSyntaxTutorial new welcome lesson.
 	self assert: displayedText equals: expected.
]

{ #category : #tests }
PharoTutorialTestGo >> testGoTwiceShowLessonViewIfNotVisible [
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	firstLessonView close.
	ProfStef goOn: PharoSyntaxTutorial.
	self assert:  (World systemWindows includes:  firstLessonView window).
]

{ #category : #tests }
PharoTutorialTestGo >> testGoTwiceUseSameLessonView [
	| firstLessonView |
	ProfStef go.
	firstLessonView := ProfStef default lessonView.
	ProfStef goOn: PharoSyntaxTutorial.
	self assert: (firstLessonView ==  ProfStef default lessonView).
]
