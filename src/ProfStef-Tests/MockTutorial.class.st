"
Mocking a tutorial for test purposes
"
Class {
	#name : #MockTutorial,
	#superclass : #AbstractTutorial,
	#category : #'ProfStef-Tests-Mocks'
}

{ #category : #lesson }
MockTutorial >> firstLesson [
	^ Lesson title: 'first' lesson: 'First lesson'.
]

{ #category : #lesson }
MockTutorial >> secondLesson [
	^ Lesson title: 'second' lesson: 'Second lesson'.
]

{ #category : #lesson }
MockTutorial >> thirdLesson [
	^ Lesson title: 'third' lesson: 'Third lesson'.
]

{ #category : #tutorial }
MockTutorial >> tutorial [ 
	^ #(
firstLesson
secondLesson
thirdLesson
)
]
