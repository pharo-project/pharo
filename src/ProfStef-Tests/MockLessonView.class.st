"
A LessonView mock for testing purposes
"
Class {
	#name : #MockLessonView,
	#superclass : #Object,
	#instVars : [
		'lesson',
		'title'
	],
	#category : #'ProfStef-Tests-Mocks'
}

{ #category : #testing }
MockLessonView >> isOpenInWindow [
	^ true
]

{ #category : #accessing }
MockLessonView >> lesson [
	^ lesson
]

{ #category : #gui }
MockLessonView >> showLesson: aLesson withTitle: aString [
	lesson := aLesson.
	title := aString.
]

{ #category : #gui }
MockLessonView >> showTutorialNode: aTutorialNode [
	lesson:= aTutorialNode lessonInstance.
	title := aTutorialNode title.
]

{ #category : #accessing }
MockLessonView >> title [
	^ title
]
